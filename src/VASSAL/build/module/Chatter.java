/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.build.module;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FontConfigurer;
import VASSAL.tools.KeyStrokeSource;

import javax.swing.*;
import javax.swing.plaf.basic.BasicTextAreaUI;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * The chat window component.  Displays text messages and
 * accepts input.  Also acts as a {@link CommandEncoder},
 * encoding/decoding commands that display message in the text area
 */
public class Chatter extends JPanel implements CommandEncoder, Buildable {

  protected JTextArea conversation;
  protected JTextField input;
  protected JScrollPane scroll = new JScrollPane
      (JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
       JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
  protected static final String MY_CHAT_COLOR = "myChatColor";
  protected static final String OTHER_CHAT_COLOR = "otherChatColor";
  protected static final String GAME_MSG_COLOR = "gameMessageColor";
  protected static final String SYS_MSG_COLOR = "systemMessageColor";

  private String handle;

  public Chatter() {
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    conversation = new JTextArea(15, 60);
    for (int i = 0; i < 15; ++i) {
      conversation.append("\n");
    }
    conversation.setEditable(false);
    conversation.setLineWrap(true);
    conversation.setWrapStyleWord(true);
    conversation.setUI(new UI());
    conversation.addComponentListener(new ComponentAdapter() {
      public void componentResized(ComponentEvent e) {
        scroll.getVerticalScrollBar().setValue(scroll.getVerticalScrollBar().getMaximum());
      }
    });
    input = new JTextField(60) {
      public boolean isManagingFocus() {
        return true;
      }
    };
    input.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        send(formatChat(e.getActionCommand()));
        input.setText("");
      }
    });
    input.setMaximumSize
        (new Dimension(input.getMaximumSize().width,
                       input.getPreferredSize().height));
    scroll.setViewportView(conversation);
    add(scroll);
    add(input);
  }

  private String formatChat(String text) {
    return "<" + GlobalOptions.getInstance().getPlayerId() + "> - " + text;
  }

  public JTextField getInputField() {
    return input;
  }

  /**
   * Display a message in the text area
   */
  public void show(String s) {
    conversation.append("\n" + s);
  }

  /** @deprecated use GlobalOptions.getPlayerId() */
  public void setHandle(String s) {
    handle = s;
  }

  /** @deprecated use GlobalOptions.getPlayerId() */
  public String getHandle() {
    return GlobalOptions.getInstance().getPlayerId();
  }

  /**
   * Set the Font used by the text area
   */
  public void setFont(Font f) {
    if (input != null) {
      if (input.getText().length() == 0) {
        input.setText("XXX");
        input.setFont(f);
        input.setText("");
      }
      else
        input.setFont(f);
    }
    if (conversation != null) {
      conversation.setFont(f);
    }
  }

  public void build(org.w3c.dom.Element e) {
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  /**
   * Expects to be added to a GameModule.  Adds itself to the
   * controls window and registers itself as a
   * {@link CommandEncoder} */
  public void addTo(Buildable b) {
    GameModule mod = (GameModule) b;
    mod.setChatter(this);
    mod.addCommandEncoder(this);
    mod.addKeyStrokeSource
        (new KeyStrokeSource(conversation,
                             WHEN_ANCESTOR_OF_FOCUSED_COMPONENT));
    mod.addKeyStrokeSource
        (new KeyStrokeSource(input, WHEN_FOCUSED));
    setHandle((String) mod.getPrefs().getOption(GameModule.REAL_NAME).getValue());

    FontConfigurer chatFont = new FontConfigurer
        ("ChatFont", "Chat Window Font: ");
    chatFont.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        setFont((Font) evt.getNewValue());
      }
    });

    mod.getControlPanel().add(this, BorderLayout.CENTER);

    chatFont.fireUpdate();
    mod.getPrefs().addOption("Chat Window", chatFont);
    ColorConfigurer gameMsgColor = new ColorConfigurer(GAME_MSG_COLOR, "Game messages", Color.magenta);
    mod.getGlobalPrefs().addOption("Chat Window", gameMsgColor);
    ColorConfigurer systemMsgColor = new ColorConfigurer(SYS_MSG_COLOR, "System messages", new Color(160, 160, 160));
    mod.getGlobalPrefs().addOption("Chat Window", systemMsgColor);
    ColorConfigurer myChatColor = new ColorConfigurer(MY_CHAT_COLOR, "My text messages", Color.gray);
    mod.getGlobalPrefs().addOption("Chat Window", myChatColor);
    ColorConfigurer otherChatColor = new ColorConfigurer(OTHER_CHAT_COLOR, "Other's text messages", Color.black);
    mod.getGlobalPrefs().addOption("Chat Window", otherChatColor);
  }

  public void add(Buildable b) {
  }

  public Command decode(String s) {
    if (s.startsWith("CHAT")) {
      return new DisplayText(this, s.substring(4));
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof DisplayText) {
      return "CHAT" + ((DisplayText) c).msg;
    }
    else {
      return null;
    }
  }

  /**
   * Displays the message, Also logs and sends to the server
   * a {@link Command} that displays this message
   */
  public void send(String msg) {
    if (msg != null
        && msg.length() > 0) {
      show(msg);
      GameModule.getGameModule().sendAndLog(new DisplayText(this, msg));
    }
  }


  /**
   * Classes other than the Chatter itself may forward KeyEvents
   * to the Chatter by using this method
   */
  public void keyCommand(KeyStroke e) {
    if ((e.getKeyCode() == 0 || e.getKeyCode() == KeyEvent.CHAR_UNDEFINED)
        && !Character.isISOControl(e.getKeyChar())
        && (e.getModifiers() & (InputEvent.CTRL_DOWN_MASK + InputEvent.ALT_DOWN_MASK + InputEvent.META_DOWN_MASK)) == 0) {
      input.setText(input.getText() + e.getKeyChar());
    }
    else if (e.isOnKeyRelease()) {
      switch (e.getKeyCode()) {
        case KeyEvent.VK_ENTER:
          if (input.getText().length() > 0)
            send(formatChat(input.getText()));
          input.setText("");
          break;
        case KeyEvent.VK_BACK_SPACE:
        case KeyEvent.VK_DELETE:
          String s = input.getText();
          if (s.length() > 0)
            input.setText(s.substring(0, s.length() - 1));
          break;
      }
    }
  }

  private class UI extends BasicTextAreaUI {
    public View create(javax.swing.text.Element elem) {
      JTextComponent c = getComponent();
      if (c instanceof JTextArea) {
        JTextArea area = (JTextArea) c;
        View v;
        if (area.getLineWrap()) {
          v = new WrappedView(elem, area.getWrapStyleWord());
        }
        else {
          v = new PView(elem);
        }
        return v;
      }
      return null;
    }
  }

  private int drawColoredText(Graphics g, int x, int y, TabExpander ex, Document doc,
                              int p0, int p1, Element elem) throws BadLocationException {
    Segment s = new Segment();
    doc.getText(p0, p1 - p0, s);
    g.setColor(getColor(elem));
    return Utilities.drawTabbedText(s, x, y, g, ex, p0);
  }

  private class WrappedView extends WrappedPlainView {
    private WrappedView(Element el, boolean wrap) {
      super(el, wrap);
    }

    protected int drawUnselectedText(Graphics g, int x, int y,
                                     int p0, int p1) throws BadLocationException {
      Element root = getElement();
      return drawColoredText(g, x, y, this, getDocument(), p0, p1, root.getElement(root.getElementIndex(p0)));
    }
  }

  private class PView extends PlainView {
    private PView(Element el) {
      super(el);
    }

    protected int drawUnselectedText(Graphics g, int x, int y,
                                     int p0, int p1) throws BadLocationException {
      Element root = getElement();
      return drawColoredText(g, x, y, this, getDocument(), p0, p1, root.getElement(root.getElementIndex(p0)));
    }
  }

  /**
   * Determines the color with which to draw a given line of text
   * @return the Color to draw
   */
  protected Color getColor(Element elem) {
    Color col = null;
    try {
      String s = elem.getDocument().getText(elem.getStartOffset(), elem.getEndOffset() - elem.getStartOffset()).trim();
      if (s.length() > 0) {
        switch (s.charAt(0)) {
          case '*':
            col = (Color) GameModule.getGameModule().getGlobalPrefs().getValue(GAME_MSG_COLOR);
            break;
          case '-':
            col = (Color) GameModule.getGameModule().getGlobalPrefs().getValue(SYS_MSG_COLOR);
            break;
          default:
            if (s.startsWith(formatChat(""))) {
              col = (Color) GameModule.getGameModule().getGlobalPrefs().getValue(MY_CHAT_COLOR);
            }
            else {
              col = (Color) GameModule.getGameModule().getGlobalPrefs().getValue(OTHER_CHAT_COLOR);
            }
            break;
        }
      }
    }
    catch (BadLocationException e) {
      e.printStackTrace();
    }
    return col == null ? Color.black : col;
  }

  /**
   * This is a {@link Command} object that, when executed, displays
   * a text message in the Chatter's text area     */
  public static class DisplayText extends Command {
    private String msg;
    private Chatter c;

    public DisplayText(Chatter c, String s) {
      this.c = c;
      msg = s;
    }

    public void executeCommand() {
      c.show(msg);
    }

    public Command myUndoCommand() {
      return new DisplayText(c, "* UNDO: " + msg);
    }

    public String getMessage() {
      return msg;
    }
  }

  public static void main(String[] args) {
    Chatter chat = new Chatter();
    chat.setHandle("test");
    JFrame f = new JFrame();
    f.getContentPane().add(chat);
    f.pack();
    f.setVisible(true);
  }
}
