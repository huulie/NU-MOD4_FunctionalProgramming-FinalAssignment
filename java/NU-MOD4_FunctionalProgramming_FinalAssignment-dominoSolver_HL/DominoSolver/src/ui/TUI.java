package ui;

import solver.BoneGrid;
import domino.Grid;
import solver.PipGrid;
import solver.Solver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;

public class TUI {

    private static Solver solver = new Solver();

    public static void main(String[] args) {
        // ask user for input, give that input to the solver and display the output
        Integer[] examplePipGrid1 = new Integer[]{
                6, 6, 2, 6, 5, 2, 4, 1,
                1, 3, 2, 0, 1, 0, 3, 4,
                1, 3, 2, 4, 6, 6, 5, 4,
                1, 0, 4, 3, 2, 1, 1, 2,
                5, 1, 3, 6, 0, 4, 5, 5,
                5, 5, 4, 0, 2, 6, 0, 3,
                6, 0, 5, 3, 4, 2, 0, 3};
        Integer[] examplePipGrid2 = new Integer[]{
                4, 2, 5, 2, 6, 3, 5, 4,
                5, 0, 4, 3, 1, 4, 1, 1,
                1, 2, 3, 0, 2, 2, 2, 2,
                1, 4, 0, 1, 3, 5, 6, 5,
                4, 0, 6, 0, 3, 6, 6, 5,
                4, 0, 1, 6, 4, 0, 3, 0,
                6, 5, 3, 6, 2, 1, 5, 3};

        printSolutions(solver.solve(PipGrid.arrayToGrid(examplePipGrid1))); // Can be used to test with examples
//        printSolutions(solver.solve(solver.PipGrid.arrayToGrid(examplePipGrid2))); // Can be used to test with examples

//        printSolutions(solver.solve(getUserInput()));
    }

    private static PipGrid getUserInput(){
        // example1: 6,6,2,6,5,2,4,1,1,3,2,0,1,0,3,4,1,3,2,4,6,6,5,4,1,0,4,3,2,1,1,2,5,1,3,6,0,4,5,5,5,5,4,0,2,6,0,3,6,0,5,3,4,2,0,3
        String inputString = getString("Provide the input to solve (comma separated, without spaces and in row-major order):");
        Integer[] inputInt = Arrays.stream(inputString.split(",")).mapToInt(Integer::parseInt).boxed().toArray(Integer[]::new);

        PipGrid input = null;
        try {
            input = PipGrid.arrayToGrid(inputInt);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println("Wrong number of pips in input: " + e);
        }
        return input;
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
        showMessage("There are " + solutions.size() + " solution(s) for the input:");

        for (int i = 0; i < solutions.size(); i++) {
            printSolution(solutions.get(i));
        }
        showMessage("[Finished] Do you want to be more awesome? Use the Haskell solver next time!");
    }

    private static void printSolution(BoneGrid boneGrid) {
        System.out.println(" ------------------------------------ ");
        for (int y = 0; y < Grid.HEIGHT; y++) {
            System.out.print(" | ");
            for (int x = 0; x < Grid.WIDTH; x++) {
                System.out.printf(" %02d ",boneGrid.getElementAt(new Grid.Position(x, y)).getNumber());
            }
            System.out.println(" | ");
        }
        System.out.println(" ------------------------------------ ");
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
