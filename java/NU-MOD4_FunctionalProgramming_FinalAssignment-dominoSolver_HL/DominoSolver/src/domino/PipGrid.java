package domino;

/**
 * board.Grid with number of pips (input for the solver)
 */
public class PipGrid extends Grid<Integer> {

    public PipGrid () {
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

    /**
     * Transforms a 1D array of Integers into a PipGrid (row-major order)
     * @param array1D to tranform
     * @return PipGrid from the array1D
     */
    public static PipGrid arrayToGrid(Integer[] array1D) {
        PipGrid pipGrid = new PipGrid();
        for(int i = 0; i < HEIGHT*WIDTH; i++) {
            pipGrid.setElementAt(array1D[i],Grid.Position.index2position(i));
        }

        return pipGrid;
    }
}
