package VASSAL.build.module;

/*
 * 
 * @author Brent Easton
 *
 * Enhanced Dice Button includes access to Internet Die Servers via the DieManager.
 *  
 */
 
import java.io.File;
import java.net.MalformedURLException;

import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

/**
 * This component places a button into the controls window toolbar.
 * Pressing the button generates random numbers and displays the
 * result in the Chatter */

public class InternetDiceButton extends DiceButton {

    protected DieManager dieManager;

    public InternetDiceButton() {
    	super();
    }

    public static String getConfigureTypeName() {
        return "Internet Dice Button";
    }

    /**
     * Ask the die manager to do our roll!
     */
    protected void DR() {
		dieManager.roll(nDice, nSides, plus, reportTotal, getConfigureName());
    }

    /**
     * Expects to be added to the DieManager.  
	 */
    
    public void addTo(Buildable parent) {
        dieManager = (DieManager) parent;
        dieManager.addDieButton(this);
        super.addTo(parent);
    }


    public void removeFrom(Buildable b) {
        ((DieManager) b).removeDieButton(this);
        super.removeFrom(b);
    }

    public HelpFile getHelpFile() {
        File dir = new File("docs");
        dir = new File(dir, "ReferenceManual");
        try {
            return new HelpFile(
                null,
                new File(dir, "GameModule.htm"),
                "#InternetDiceButton");
        }
        catch (MalformedURLException ex) {
            return null;
        }
    }
}

