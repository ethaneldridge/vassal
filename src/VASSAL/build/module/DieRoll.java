package VASSAL.build.module;

/*
 * 
 * @author Brent Easton
 *
 * Describes a single roll of one or more identical dice.
 * 
 */
public class DieRoll {

	protected String description = "";
	protected int numDice;
	protected int numSides;
	protected int plus;
	protected boolean reportTotal; 
	protected int[] result;

	public DieRoll(String d, int dice, int sides, int add, boolean r) {
		description = d;
		numDice = dice;
		numSides = sides;
		plus = add;
		reportTotal = r;
		result = new int[dice];
	}


	public DieRoll(String d, int dice, int sides, int add) {
		this(d, dice, sides, add, false);
	}

	public DieRoll(String d, int dice, int sides) {
		this(d, dice, sides, 0);
	}

	public int[] getResults() {
		return result;
	}

	public int getResult(int pos) {
		return result[pos];
	}

	public void setResult(int pos, int res) {
		result[pos] = res;
	}
	
	public void setNumDice (int nd) {
		numDice = nd;
		result = new int[nd];
	}
}