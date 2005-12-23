/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package Dev;

import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;
import java.util.regex.Pattern;

import org.nfunk.jep.JEP;
import org.nfunk.jep.Operator;
import org.nfunk.jep.OperatorSet;
import org.nfunk.jep.ParseException;
import org.nfunk.jep.SymbolTable;
import org.nfunk.jep.Variable;
import org.nfunk.jep.function.Add;
import org.nfunk.jep.function.Comparative;
import org.nfunk.jep.function.PostfixMathCommand;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.PlayerRoster;
import VASSAL.counters.GamePiece;

/**
 * @author Brent Easton
 * 
 * Implementation of a General Purpose mathematical expression parser and
 * evaluator. Core functionality provide by the Java Expression Parser (JEP)
 * package. Custom extensions to JEP: 
 * - Allow Numbers to be compared with Strings. 
 * - Allow Numbers to be concatenated with Strings. 
 * - Support for VASSAL "true" and "false" boolean property values. 
 * - matches() function to do Regular expression pattern matching.
 * - Maintain a LRU cache of parser instances to minimise overheads involved
 *   in re-parsing expressions.
 */
public class ExpressionEvaluator {

  protected static final int PARSER_CACHE_SIZE = 10;
  
  protected static ExpressionEvaluator instance = null;
  protected static ParserCache parserCache;

  static {
    instance = new ExpressionEvaluator();
  }
  
  public ExpressionEvaluator() {
    parserCache = new ParserCache(PARSER_CACHE_SIZE);
  }

  /**
   * Evaluate an expression and return a String result.
   * 
   * @param expression
   *          Expression to evaluate
   * @param piece
   *          GamePiece to use to supply values for named properties
   * @param errorResult
   *          Value to return on error
   * @return Evaluated expression result
   */
  public static String evaluate(String expression, GamePiece piece, String errorResult) {

    /*
     * Hopefully this expression has already been parser and is ready
     * to go in our cache.
     */
    JEP parser = parserCache.find(expression);
    
    /*
     * Clear any variables from last evaluation
     */
    clearVariables(parser);

     /*
     * Replace any undeclared variables with their values based on getProperty()
     * calls on the target GamePiece
     */
    bind(parser, piece);

    /*
     * Evaluate the expression
     */
    return evaluate(parser, errorResult);
  }
  
  public static String evaluate(String expression, GamePiece piece) {
    return evaluate(expression, piece, "");
  }
  
  /*
   * Clear the variables in the symbol table from the previous evaluation.
   */
  protected static void clearVariables(JEP parser) {
    parser.getSymbolTable().clearValues();
  }
  
  /*
   * Bind undeclared variables to getProperty() calls on
   * the target GamePiece
   */
  public static void bind(JEP parser, GamePiece piece) {
    SymbolTable table = parser.getSymbolTable();
    Enumeration e = table.elements();

    while (e.hasMoreElements()) {
      Variable var = (Variable) e.nextElement();
      if (!var.hasValidValue()) {
        String varName = var.getName();
        String val = (String) piece.getProperty(varName);
        try {
          Double d = new Double(val);
          parser.setVarValue(varName, d);
        }
        catch (Exception ex) {
          parser.setVarValue(varName, val);
        }
      }
    }
  }
  
  /*
   * Evaluate the expression
   */
  public static String evaluate(JEP parser, String errorResult) {
    Object result = parser.getValueAsObject();

    /*
     * Return specified value if any errors, otherwise
     * Convert returned numbers to Strings for VASSAL
     */
    if (parser.hasError()) {
      return errorResult;
    }
    else if (result instanceof Double) {
      return numberToString((Double) result);
    }
    else if (result instanceof String) {
      return (String) result;
    }
    return "";
  }
  
  /**
   * Evaluate an expression and return a boolean result. Use for evaluating
   * expressions known to be boolean (e.g. PropertiesPieceFilter). JEP will
   * return 0 for false, 1 for true.
   * 
   * @param expression
   *          Expression to evaluate
   * @param piece
   *          GamePiece to use to supply values for named properties
   * @param errorResult
   *          Value to return on error
   * @return Evaluated expression result
   */
  public static boolean evaluateBoolean(String expression, GamePiece piece, boolean errorResult) {
    String result = evaluate(expression, piece, errorResult ? "1" : "0");
    return result.equals("1");
  }

  public static boolean evaluateBoolean(String expression, GamePiece piece) {
    return evaluateBoolean(expression, piece, false);
  }
  
  /**
   * 
   * JEP parser. Add VASSAL constants, functions and use custom Operator Set
   */
  protected class VassalJEP extends JEP {

    protected String expression = "";
  
    public VassalJEP() {
      super();
      addStandardFunctions();
      addStandardConstants();
      addConstant("true", "true");
      addConstant("false", "false");
      addConstant(GlobalOptions.PLAYER_NAME, (String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
      addConstant(GlobalOptions.PLAYER_SIDE,PlayerRoster.getMySide());
      addConstant(GlobalOptions.PLAYER_ID,GlobalOptions.getInstance().getPlayerId());
      addFunction("matches", new Matches());
      setAllowUndeclared(true);
      opSet = new VassalOperatorSet();
    }
    
    public VassalJEP(String e) {
      this();
      expression = e;
      parseExpression(expression);
    }

  }

  /**
   * Custom OperatorSet. Use custom Comparative class for all comparison
   * operators Use custom Add class for '+'
   */
  protected class VassalOperatorSet extends OperatorSet {

    public VassalOperatorSet() {
      super();
      OP_GT = new Operator(">", new VassalComparative(Comparative.GT));
      OP_LT = new Operator("<", new VassalComparative(Comparative.LT));
      OP_EQ = new Operator("==", new VassalComparative(Comparative.EQ));
      OP_LE = new Operator("<=", new VassalComparative(Comparative.LE));
      OP_GE = new Operator(">=", new VassalComparative(Comparative.GE));
      OP_NE = new Operator("!=", new VassalComparative(Comparative.NE));
      OP_ADD = new Operator("+", new VassalAdd());
    }

  }

  /**
   * Custom Comparative class. Allow numbers to be compared to Strings by
   * converting the number to a String and doing a lexical comparison.
   */
  protected class VassalComparative extends Comparative {

    public VassalComparative(int id_in) {
      super(id_in);
    }

    public boolean lt(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof String)) {
        return ((String) param1).compareTo((String) param2) < 0;
      }
      else if ((param1 instanceof String) && (param2 instanceof Number)) {
        return ((String) param1).compareTo(numberToString((Number) param2)) < 0;
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return numberToString((Number) param2).compareTo((String) param2) < 0;
      }
      else {
        return super.lt(param1, param2);
      }
    }

    public boolean gt(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof String)) {
        return ((String) param1).compareTo((String) param2) > 0;
      }
      else if ((param1 instanceof String) && (param2 instanceof Number)) {
        return ((String) param1).compareTo(numberToString((Number) param2)) > 0;
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return numberToString((Number) param2).compareTo((String) param2) > 0;
      }
      else {
        return super.gt(param1, param2);
      }
    }

    public boolean le(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof String)) {
        return ((String) param1).compareTo((String) param2) <= 0;
      }
      else if ((param1 instanceof String) && (param2 instanceof Number)) {
        return ((String) param1).compareTo(numberToString((Number) param2)) <= 0;
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return numberToString((Number) param2).compareTo((String) param2) <= 0;
      }
      else {
        return super.le(param1, param2);
      }
    }

    public boolean ge(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof String)) {
        return ((String) param1).compareTo((String) param2) >= 0;
      }
      else if ((param1 instanceof String) && (param2 instanceof Number)) {
        return ((String) param1).compareTo(numberToString((Number) param2)) >= 0;
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return numberToString((Number) param2).compareTo((String) param2) >= 0;
      }
      else {
        return super.ge(param1, param2);
      }
    }

    public boolean eq(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof Number)) {
        return param1.equals(numberToString((Number) param2));
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return numberToString((Number) param1).equals(param2);
      }
      else {
        return super.eq(param1, param2);
      }
    }

    public boolean ne(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof Number)) {
        return !param1.equals(numberToString((Number) param2));
      }
      else if ((param1 instanceof Double) && (param2 instanceof String)) {
        return !numberToString((Number) param1).equals(param2);
      }
      else {
        return super.ne(param1, param2);
      }
    }
  }

  /**
   * Customised Add class. 
   * Allow numbers to be concatenated with Strings.
   */
  protected class VassalAdd extends Add {
    public Object add(Object param1, Object param2) throws ParseException {
      if ((param1 instanceof String) && (param2 instanceof Number)) {
        return (String) param1 + numberToString((Number) param2);
      }
      else if ((param1 instanceof Number) && (param2 instanceof String)) {
        return numberToString((Number) param1) + (String) param2;
      }
      else {
        return super.add(param1, param2);
      }
    }
  }

  /**
   * 
   * Convert numbers to Strings with any special formatting needed. Remove the
   * trailing ".0" from Double's so they compare with equivalentStrings. NOTE:
   * JEP uses Double internally for all standard numbers.
   */
  public static String numberToString(Number n) {
    if (n instanceof Double) {
      return numberToString(((Double) n).doubleValue());
    }
    else {
      return n.toString();
    }
  }

  public static String numberToString(double d) {
    String s = Double.toString(d);
    if (s.endsWith(".0")) {
      s = s.substring(0, s.length() - 2);
    }
    return s;
  }

  /**
   * 
   * User defined function - Regular pattern matching
   * matches(<regexp>, value)
   */
  public class Matches extends PostfixMathCommand {
    public Matches() {
      numberOfParameters = 2;
    }

    public void run(Stack inStack) throws ParseException {
      checkStack(inStack);// check the stack
      Object param2 = inStack.pop();
      Object param1 = inStack.pop();

      if ((param1 instanceof String) && (param2 instanceof String)) {
        inStack.push(Pattern.matches((String) param1, (String) param2) ? new Double(1) : new Double(0));
      }
      else {
        throw new ParseException("Matches: Invalid parameter types " + param1.getClass().getName() + " "
            + param1.getClass().getName());
      }
      return;
    }
  }
  
  /**
   * 
   * Least Recently Used cache of JEP parser instances. Try and minimise 
   * the number of times parsers are instantiated and expressions parsed.
   */
  public class ParserCache extends LinkedHashMap {
    
    protected int cacheSize;
    
    public ParserCache (int cacheSize) {
      super(cacheSize);
      this.cacheSize = cacheSize;
    }
    
    protected boolean removeEldestEntry(Map.Entry entry) {
      return this.size() > cacheSize;
    }
    
    /*
     * Find or create parser and move it to the front of
     * the cache
     */
    public JEP find(String expression) {
      JEP parser = (JEP) remove(expression);
      if (parser == null) {
        parser = new VassalJEP(expression);
      }
      put(expression, parser);
      return parser;
    }
 
  }
}
