package avl;

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.GridEditor;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering;

public class TerrainHexGridEditor extends GridEditor implements ActionListener {

  private static final long serialVersionUID = 1L;
  
  protected static final String BLANK = "Blank";
  protected static final String HEX = "Hex";
  protected static final String EDGE = "Edge";
  protected static final String LINE = "Line";
  protected static final String GRID = "Grid";
  protected static final String OPTIONS = "Options";
  protected static final String NUMBERING = "Numbering";
  
  protected JPanel buttons;
  protected TerrainHexGrid myGrid;
  protected TerrainMap terrainMap;
  protected TerrainDefinitions terrain;
  protected ArrayList selectedHexList = new ArrayList();
  protected ArrayList selectedEdgeList = new ArrayList();
  protected Area selectedHexArea = new Area();
  protected Area selectedEdgeArea = new Area();

  protected JButton hexButton, edgeButton, connectButton, gridButton;
  protected JPanel cardPanel;
  protected CardLayout cardLayout;
  protected String mode;
  
  public TerrainHexGridEditor(EditableGrid grid) {
    super(grid);
    myGrid = (TerrainHexGrid) grid;
    terrainMap = getTerrainDefs().getTerrainMap(myGrid);
  }

  protected TerrainDefinitions getTerrainDefs() {
    if (terrain == null) {
      terrain = TerrainDefinitions.getInstance(); 
    }
    return terrain;
  }
  
  public void mouseClicked(MouseEvent e) {
    if (isGridMode()) {
      super.mouseClicked(e);
    }
    else {
      if (HEX.equals(mode)) {

        if (!e.isShiftDown() && selectedHexList.size() > 0) {
          clearHexSelection();
        }
        
        HexRef hexPos = new HexRef(myGrid.getHexPos(e.getPoint()));
        Area hex = myGrid.getSingleHex(e.getPoint());
        
        int index = selectedHexList.indexOf(hexPos);

        if (index >= 0) {
          // Already in list, remove if shiftdown
          if (e.isShiftDown()) {
            selectedHexList.remove(index);
            selectedHexArea.subtract(hex);
            view.repaint(hex.getBounds());
          }
        }
        else {
          selectedHexList.add(hexPos);
          selectedHexArea.add(hex);
          view.repaint(hex.getBounds());
        }
      }
      else if (EDGE.equals(mode)) {

        if (!e.isShiftDown() && selectedEdgeList.size() > 0) {
          clearEdgeSelection();
        }
        
        Point p = e.getPoint();
        Point snap = myGrid.snapToHexSide(p);
        Point snapCentre = myGrid.snapToHex(p);
        
        // Ignore snaps to hex centres
        if (snap.equals(snapCentre)) {
          return;
        }
        
        int index = selectedEdgeList.indexOf(snap);
        Area area = new Area(new Ellipse2D.Double(snap.x-5, snap.y-5, 11, 11));
        
        if (index >= 0) {
          // Already in list, remove if shiftdown
          if (e.isShiftDown()) {
            selectedEdgeList.remove(index);
            selectedEdgeArea.subtract(area);
            view.repaint(area.getBounds());
          }
        }
        else {
          selectedEdgeList.add(snap);
          selectedEdgeArea.add(area);
          view.repaint(area.getBounds());
        }
      }
    }
  }
  
  public void clearHexSelection() {
    Rectangle update = selectedHexArea.getBounds();
    selectedHexList.clear();
    selectedHexArea = new Area();
    view.repaint(update);
  }
  
  public void clearEdgeSelection() {
    Rectangle update = selectedEdgeArea.getBounds();
    selectedEdgeList.clear();
    selectedEdgeArea = new Area();
    view.repaint(update);
  }
  
  protected void setSelectedHexTerrain(String terrainName) {
    terrainMap.setHexTerrainType(selectedHexList, (HexTerrain) getTerrainDefs().getHexTerrainDefinitions().getTerrain(terrainName));
  }

  protected void setSelectedEdgeTerrain(String terrainName) {
    terrainMap.setEdgeTerrainType(selectedEdgeList, myGrid, (EdgeTerrain) getTerrainDefs().getEdgeTerrainDefinitions().getTerrain(terrainName));
  }
  
  public void actionPerformed(ActionEvent e) {
    String option = e.getActionCommand();
    if (OK.equals(option)) {
      cancelSetMode();
      setMode(BLANK);
      terrainMap.save();
      setVisible(false);
    }
    else if (SET.equals(option)) {
      startSetMode();
    }
    else if (NUMBERING.equals(option)) {
      ((RegularGridNumbering) grid.getGridNumbering()).setAttribute(RegularGridNumbering.VISIBLE, new Boolean(! grid.getGridNumbering().isVisible()));
      repaint();
    }
    else if (CANCEL_SET.equals(option)) {
      cancelSetMode();
    }
    else if (CANCEL.equals(option)) {
      cancelSetMode();
      grid.setDx(saveDx);
      grid.setDy(saveDy);
      grid.setOrigin(saveOrigin);
      setVisible(false);
    }
    else if (HEX.equals(option)) {
      clearEdgeSelection();
      setMode(option);
    }
    else if (EDGE.equals(option)) {
      clearHexSelection();
      setMode(option);
    }
    else if (LINE.equals(option)) {
      clearHexSelection();
      clearEdgeSelection();
      setMode(option);
    }
    else if (GRID.equals(option)) {
      clearHexSelection();
      clearEdgeSelection();
      setMode(option);      
    }
    view.requestFocus();
    return;
  }  

  protected void initComponents() {
    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    view = new TerrainGridPanel(board);

    view.addMouseListener(this);
    view.addKeyListener(this);
    view.setFocusable(true);

    scroll =
        new JScrollPane(
            view,
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

    scroll.setPreferredSize(new Dimension(800, 600));
    getContentPane().add(scroll, BorderLayout.CENTER);

    cardLayout = new CardLayout();
    cardPanel = new JPanel(cardLayout);
    cardPanel.setBorder(BorderFactory.createEtchedBorder());

    JPanel blankPanel = new JPanel();
    cardPanel.add(blankPanel, BLANK);  
    
    JPanel bottomPanel = new JPanel(new BorderLayout());
    
    JPanel modePanel = new JPanel(new BorderLayout());
    modePanel.setBorder(BorderFactory.createEtchedBorder());
    modePanel.add(new JLabel("Mode"), BorderLayout.NORTH);
    
    JPanel buttonPanel = new JPanel();
    Box leftButtonBox = Box.createVerticalBox();
    Box rightButtonBox = Box.createVerticalBox();
    
    /*
     * Hex Terrain Mode button and panel
     */
    hexButton = new JButton(HEX);
    hexButton.addActionListener(this);
    leftButtonBox.add(hexButton);
    JPanel hexPanel = new HexPanel();
    cardPanel.add(hexPanel, HEX);
    
    /*
     * Edge Terrain Mode button and panel
     */
    edgeButton = new JButton(EDGE);
    edgeButton.addActionListener(this);
    leftButtonBox.add(edgeButton);
    JPanel edgePanel = new EdgePanel();
    cardPanel.add(edgePanel, EDGE);

    /*
     * Connection Terrain Mode button and panel
     */
    connectButton = new JButton(LINE);
    connectButton.addActionListener(this);
    leftButtonBox.add(connectButton);
    JPanel connectPanel = new JPanel();
    connectPanel.add(new JLabel(LINE));
    cardPanel.add(connectPanel, LINE);
    
    /*
     * Save Button
     */
    okButton = new JButton(OK);
    okButton.addActionListener(this);
    leftButtonBox.add(okButton);
     
    /*
     * Set Grid Mode button and panel
     */
    gridButton = new JButton(GRID);
    gridButton.addActionListener(this);
    rightButtonBox.add(gridButton);
    
    Box gridPanel = Box.createVerticalBox();
    gridPanel.add(new JLabel("Arrow Keys - Move Grid"));
    gridPanel.add(new JLabel("Control-Arrow Keys - Resize Grid"));
    gridPanel.add(new JLabel("Shift Key - Increase speed of other keys"));

    setButton = new JButton(SET);
    setButton.addActionListener(this);
    gridPanel.add(setButton);
    
    canSetButton = new JButton(CANCEL_SET);
    canSetButton.addActionListener(this);
    canSetButton.setVisible(false);
    gridPanel.add(canSetButton);
    cardPanel.add(gridPanel, GRID);
    
    /*
     * Toggle Grid Numbering display button
     */
    numberingButton = new JButton(NUMBERING);
    numberingButton.addActionListener(this);
    numberingButton.setEnabled(grid.getGridNumbering() != null);
    numberingButton.setVisible(true);
    rightButtonBox.add(numberingButton);
    
    /*
     * Options button and panel
     */
    JButton optionsButton = new JButton(OPTIONS);
    optionsButton.addActionListener(this);
    rightButtonBox.add(optionsButton);
    JPanel optionsPanel = new JPanel();
    optionsPanel.add(new JLabel(OPTIONS));
    cardPanel.add(optionsPanel, OPTIONS);
    
    /*
     * Cancel button
     */
    JButton canButton = new JButton(CANCEL);
    canButton.addActionListener(this);
    rightButtonBox.add(canButton); 
    
    buttonPanel.add(leftButtonBox);
    buttonPanel.add(rightButtonBox);
    modePanel.add(buttonPanel, BorderLayout.SOUTH);
    
    bottomPanel.add(modePanel, BorderLayout.WEST);  

    mode = BLANK;

    bottomPanel.add(cardPanel, BorderLayout.CENTER);
    getContentPane().add(bottomPanel, BorderLayout.SOUTH);

    board.fixImage(view);
    scroll.revalidate();
    pack();
    repaint();
  }

  protected void setMode(String m) {
    if (isGridMode() && !m.equals(GRID)) {
      cancelSetMode();
    }
    mode = m;
    cardLayout.show(cardPanel, m);
  }
  
  protected boolean isGridMode() {
    return mode.equals(GRID);
  }
  
  public void keyPressed(KeyEvent e) {  
    if (isGridMode()) {
      super.keyPressed(e);
    }
  }
  
  public void paintTerrain(Graphics g) {
    Graphics2D g2 = (Graphics2D) g;
    Color oldColor = g.getColor();
    Composite oldComposite = g2.getComposite();
    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
    
    String[] terrainNames = TerrainDefinitions.getInstance().getHexTerrainDefinitions().getTerrainNames();
    for (int i=0; i < terrainNames.length; i++) {
      String type = terrainNames[i];
      Area area = terrainMap.getHexArea(type);
      if (area != null) {
        Color color = TerrainDefinitions.getInstance().getHexTerrainDefinitions().getTerrain(type).getColor();
        g.setColor(color);
        g2.fill(area);
      }
    }
    
    terrainNames = TerrainDefinitions.getInstance().getEdgeTerrainDefinitions().getTerrainNames();
    for (int i=0; i < terrainNames.length; i++) {
      String type = terrainNames[i];
      Area area = terrainMap.getEdgeArea(type);
      if (area != null) {
        Color color = TerrainDefinitions.getInstance().getEdgeTerrainDefinitions().getTerrain(type).getColor();
        g2.setColor(color);
        g2.fill(area);
      }
    }
    
    g2.setComposite(oldComposite);
    g.setColor(oldColor);
  }

  public void paintSelected(Graphics g) {
    if (selectedHexList.size() > 0) {
      Graphics2D g2 = (Graphics2D) g;
      Color oldColor = g.getColor();
      Composite oldComposite = g2.getComposite();
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.75f));
      g.setColor(Color.red);
      g2.fill(selectedHexArea);
      g2.setComposite(oldComposite);
      g.setColor(oldColor);
    }
    
    if (selectedEdgeList.size() > 0) {
      Graphics2D g2 = (Graphics2D) g;
      Color oldColor = g.getColor();
      Composite oldComposite = g2.getComposite();
      g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.75f));
      g.setColor(Color.red);
      g2.fill(selectedEdgeArea);
      g2.setComposite(oldComposite);
      g.setColor(oldColor);
    } 
  }

  /*
   * ---------------------------------------------------------------------
   * Following code extracted from HexGrid.HexGridEditor
   */
  /*
   * Calculate approximate grid metrics based on the three adjacent points
   * picked out by the user.
   */
  public void calculate() {

    /*
     * Two of the points must lie on the same horizontal or vertical line (be
     * perpendicular to). The third point must not be perpendicular to either of
     * the first two. First step is to work out which is which as we can't be
     * sure what order they picked out the points in.
     */

    if (isPerpendicular(hp1, hp2)) {
      calculate_step2(hp1, hp2, hp3);
    }
    else if (isPerpendicular(hp1, hp3)) {
      calculate_step2(hp1, hp3, hp2);
    }
    else if (isPerpendicular(hp2, hp3)) {
      calculate_step2(hp2, hp3, hp1);
    }
    else {
      reportShapeError();
    }
  }

  /*
   * Step 2. Check third point is not perpendicular to either of the first two,
   * then call appropriate calculation routine depending on location relative to
   * the first two.
   */
  protected void calculate_step2(Point p1, Point p2, Point p3) {
    if (!isPerpendicular(p1, p3) && !isPerpendicular(p2, p3)) {
      if (isHorizontal(p1, p2)) {
        if ((p3.x < p1.x && p3.x < p2.x) || (p3.x > p1.x && p3.x > p2.x)) {
          check(false, p1, p2, p3);
        }
        else {
          checkEnd(true, p1, p2, p3);
        }
      }
      else {
        if ((p3.y < p1.y && p3.y < p2.y) || (p3.y > p1.y && p3.y > p2.y)) {
          check(true, reverse(p1), reverse(p2), reverse(p3));
        }
        else {
          checkEnd(false, reverse(p1), reverse(p2), reverse(p3));
        }
      }
    }
    else {
      reportShapeError();
    }
  }

  protected Point reverse(Point p) {
    return new Point(p.y, p.x);
  }

  protected void check(boolean sideways, Point p1, Point p2, Point p3) {

    int r = Math.abs(p1.x - p2.x);
    int width = r * 3 / 2;
    int height = Math.abs(p3.y - p2.y) * 2;

    int Xoff = (Math.min(p1.x, p2.x)) % width + (int) (r / 2);
    int col = (int) (Math.min(p1.x, p2.x) / width);
    int Yoff = Math.min(p1.y, p2.y) % height - ((col % 2 == 1) ? 0 : (int) (height / 2));
    if (Yoff < 0)
      Yoff += height;

    setMetrics(width, height, Xoff, Yoff, sideways);
  }

  protected void checkEnd(boolean sideways, Point p1, Point p2, Point p3) {
    if (Math.abs((p1.x + p2.x) / 2 - p3.x) > ERROR_MARGIN) {
      reportShapeError();
      return;
    }

    int r = Math.abs(p3.y - p1.y) * 2;
    int width = r * 3 / 2;
    int height = Math.abs(p3.x - p2.x) * 2;

    int xOrigin = p1.y - (p3.y < p1.y ? 0 : r);
    int Xoff = xOrigin % width + (int) (r / 2);
    int col = (int) (xOrigin / width);
    int Yoff = Math.min(p1.x, p2.x) % height - ((col % 2 == 1) ? 0 : (int) (height / 2));

    setMetrics(width, height, Xoff, Yoff, sideways);
  }

  protected void setMetrics(int width, int height, int xoff, int yoff, boolean b) {

    grid.setDx(width);
    grid.setDy(height);
    grid.setOrigin(new Point(xoff, yoff));
    grid.setSideways(b);

  }

  protected class TerrainGridPanel extends GridEditor.GridPanel {

    private static final long serialVersionUID = 1L;
    protected Board board;

    public TerrainGridPanel() {
      super();
    }

    public TerrainGridPanel(Board b) {
      super(b);
    }

    public void paint(Graphics g) {
      super.paint(g);
      paintTerrain(g);
      paintSelected(g);
    }

  }

  /*
   * Panel of Terrain Selection buttons for Hex Mode
   */
  protected class HexPanel extends JPanel implements ActionListener {

    private static final long serialVersionUID = 1L;
    
    protected ArrayList buttons = new ArrayList();

    public HexPanel() {
      super();
      add(new JLabel("HEX MODE"));
      init();
    }
    
    public void init() {
      
      Iterator it = buttons.iterator();
      while (it.hasNext()) {
        remove((JButton) it.next());
      }
      buttons.clear();
      
      String[] terrainNames = getTerrainDefs().getHexTerrainDefinitions().getTerrainNames();
      Icon[] terrainIcons = getTerrainDefs().getHexTerrainDefinitions().getTerrainIcons();
      
      for (int i=0; i<terrainNames.length; i++) {
        JButton button;
        if (terrainNames[i] == null) {
          button = new JButton(terrainNames[i]);
        }
        else {
          button = new JButton(terrainNames[i], terrainIcons[i]);
        }
        button.addActionListener(this);
        add(button);
        buttons.add(button);
      }
    }

    public void actionPerformed(ActionEvent e) {
      setSelectedHexTerrain(e.getActionCommand());
      clearHexSelection();
    }
    
  }
  
  /*
   * Panel of Terrain Selection buttons for Edge Mode
   */
  protected class EdgePanel extends JPanel implements ActionListener {

    private static final long serialVersionUID = 1L;
    
    protected ArrayList buttons = new ArrayList();

    public EdgePanel() {
      super();
      add(new JLabel("EDGE MODE"));
      init();
    }
    
    public void init() {
      
      Iterator it = buttons.iterator();
      while (it.hasNext()) {
        remove((JButton) it.next());
      }
      buttons.clear();
      
      String[] terrainNames = getTerrainDefs().getEdgeTerrainDefinitions().getTerrainNames();
      Icon[] terrainIcons = getTerrainDefs().getEdgeTerrainDefinitions().getTerrainIcons();
      
      for (int i=0; i<terrainNames.length; i++) {
        JButton button;
        if (terrainNames[i] == null) {
          button = new JButton(terrainNames[i]);
        }
        else {
          button = new JButton(terrainNames[i], terrainIcons[i]);
        }
        button.addActionListener(this);
        add(button);
        buttons.add(button);
      }
    }

    public void actionPerformed(ActionEvent e) {
      setSelectedEdgeTerrain(e.getActionCommand());
      clearEdgeSelection();
    }
    
  }
}
