package vip;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.KeyStroke;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.tools.KeyStrokeListener;

  public class MiniDiceButton extends DiceButton {
    
    public MiniDiceButton() {
      super();
    }
    
    public void addTo(Buildable parent) {
      ran = GameModule.getGameModule().getRNG();
      KeyStrokeListener keyListener = new KeyStrokeListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          doRoll();
        }
      });
      KeyStroke key = HotKeyConfigurer.decode(launch.getAttributeValueString(HOTKEY) + "");
      keyListener.setKeyStroke(key);
      GameModule.getGameModule().addKeyStrokeListener(keyListener);
    }
    
    public void removeFrom(Buildable b) {
      
    }
    
    public void doRoll() {
      launch.doClick();
    }
  }