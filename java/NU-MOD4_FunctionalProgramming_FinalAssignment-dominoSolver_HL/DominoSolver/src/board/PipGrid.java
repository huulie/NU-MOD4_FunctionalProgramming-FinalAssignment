package board;

public class PipGrid extends Grid {

    /**
     * board.Grid with number of pips (input for the solver)
     */
    private int[][] pips;

    PipGrid () {
        pips = new int[WIDTH][HEIGHT];
    }

    private void setPip(Position position, int pip) {
        pips[position.getX()][position.getY()] = pip;
    }

    public int getPip (Position position) {
        return pips[position.getX()][position.getY()];
    }

    public static PipGrid arrayToGrid(int[] array1D) {
        PipGrid pipGrid = new PipGrid();
        for(int i = 0; i < HEIGHT*WIDTH; i++) {
            pipGrid.setPip(Grid.Position.index2position(i),array1D[i]);
        }

        return pipGrid;
    }
}
