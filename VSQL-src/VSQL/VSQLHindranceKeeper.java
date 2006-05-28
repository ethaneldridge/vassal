package VSQL;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;

import VASL.build.module.map.HindranceKeeper;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;

public class VSQLHindranceKeeper extends HindranceKeeper {

  protected VSQLMap vmap;
  
  public VSQLHindranceKeeper () {
    super();
  }
  
  public void addTo(Buildable b) {
    vmap = (VSQLMap) b;
    super.addTo(b);
  }
  /* 
   * Don't show hindrance counters unless LOS is activated
   */
  public void draw(Graphics g, Map m) {
    if (vmap.isLOSactivated()) {
      Graphics2D g2d = (Graphics2D) g;
      Composite oldComposite = g2d.getComposite();
      g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, vmap.getPieceOpacity()));
      super.draw(g, m);
      g2d.setComposite(oldComposite);
    }
  }
}
