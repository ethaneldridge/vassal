/*
 * Created on Oct 25, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
 package PB;

/**
 * @author Admin
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class PB {
  
  public static final String ALLIED  = "Allied";
  public static final String RUSSIAN = "Russian";
  public static final String GERMAN  = "German";
  public static final String ARAB    = "Arab";
  public static final String ISRAELI = "Israeli";
  
  public static final String ALLIED_PFX  = "al";
  public static final String RUSSIAN_PFX = "ru";
  public static final String GERMAN_PFX  = "ge";
  public static final String ARAB_PFX    = "ar";
  public static final String ISRAELI_PFX = "is";
  
  private static String[] countryList = new String[] { ALLIED, RUSSIAN, GERMAN, ARAB, ISRAELI };
  private static String[] prefixList = new String[] { ALLIED_PFX, RUSSIAN_PFX, GERMAN_PFX, ARAB_PFX, ISRAELI_PFX };
    
  public static String[] getSides() {
    return countryList;
  }
  
  public static String getSide(String prefix) {

    for (int i = 0;i < prefixList.length; i++) {
      if (prefixList[i].equals(prefix)) {
        return countryList[i];
      }
    }
    return "";
  }

  public static String getPrefix(String country) {

    for (int i = 0;i < countryList.length; i++) {
      if (countryList[i].equals(country)) {
        return prefixList[i];
      }
    }
    return "";
  }
  
}
