package ui;

import domino.BoneGrid;
import domino.Grid;
import domino.PipGrid;
import solver.Solver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;

public class TUI {

    private final static Solver solver = new Solver();

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

//        printSolutions(solver.solve(PipGrid.arrayToGrid(examplePipGrid1))); // Can be used to test with examples
//        printSolutions(solver.solve(domino.PipGrid.arrayToGrid(examplePipGrid2))); // Can be used to test with examples
        printSolutions(solver.solve(getUserInput())); // or enable this to solve user input
    }

    private static PipGrid getUserInput(){
        String inputString = getString("Provide the input to solve (comma separated, without spaces and in row-major order):");
        inputString = inputString.replaceAll("\\s",""); // to be sure: remove whitespace (in case somebody didn't read properly)
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
     * [Note: input not sanitized]
     * @param question the question shown to the user, asking for input
     * @return The user input as a String
     */
    private static String getString(String question) {
        System.out.println(question);
        String answer = null;
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            answer = in.readLine();
        } catch (IOException e) {
            System.err.println("IO exception: " + e.getLocalizedMessage());
        }
        return (answer == null) ? "" : answer;
    }

    private static void printSolutions(List<BoneGrid> solutions) {
        System.out.println("There are " + solutions.size() + " solution(s) for the input:");

        for (BoneGrid solution : solutions) {
            printSolution(solution);
        }
        System.out.println("[Finished] ## Do you want to be more awesome? Use the Haskell solver next time!");
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
}
