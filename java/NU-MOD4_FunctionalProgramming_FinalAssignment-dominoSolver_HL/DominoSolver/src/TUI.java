import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;

public class TUI {

    private static Solver solver = new Solver();

    public static void main(String[] args) {
        // ask user for input, give that input to the solver and display the output
        printSolutions(solver.solve(getUserInput()));
    }

    private static PipGrid getUserInput(){
        String inputString = getString("Provide the input to solve:");
        int[] inputInt = Arrays.stream(inputString.split(",")).mapToInt(Integer::parseInt).toArray();
        return PipGrid.arrayToGrid(inputInt);
    }

    /**
     * Prints the question and asks the user to input a String.
     *
     * @param question the question shown to the user, asking for input
     * @return The user input as a String
     */
    public static String getString(String question) {
        showMessage(question); // manual new line, for better layout (no extra white lines)
        String antw = null;
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            antw = in.readLine();
        } catch (IOException e) {
            showMessage("IO exception: " + e.getLocalizedMessage());
        }
        return (antw == null) ? "" : antw;
    }

    private static void printSolutions(List<BoneGrid> solutions) {
        showMessage(".. print solutions here .."); // TODO types solutions
    }

    /**
     * Writes the given message to system output.
     *
     * @param message the message to write to the system output.
     */
    public static void showMessage(String message) {
        System.out.println(message);
    }
}
