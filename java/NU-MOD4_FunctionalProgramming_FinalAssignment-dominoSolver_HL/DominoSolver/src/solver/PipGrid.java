package solver;

import domino.Grid;

public class PipGrid extends Grid<Integer> {

    /**
     * board.Grid with number of pips (input for the solver)
     */
    private int[][] elements;

    PipGrid () {
        super(new Integer[WIDTH][HEIGHT]);
    }

    @Override
    public Integer getElementAt(Position position) {
        return super.getElementAt(position);
    }

    @Override
    public void setElementAt(Integer pip, Position position) {
        super.setElementAt(pip, position);
    }

    // TODO move to superclass Grid?!
    public static PipGrid arrayToGrid(Integer[] array1D) {
        PipGrid pipGrid = new PipGrid();
        for(Integer i = 0; i < HEIGHT*WIDTH; i++) {
            pipGrid.setElementAt(array1D[i],Grid.Position.index2position(i));
        }

        return pipGrid;
    }
}
