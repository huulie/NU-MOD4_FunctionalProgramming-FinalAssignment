public class PipGrid extends Grid {


    /**
     * Grid with number of pips (input for the solver)
     */
    private int[][] pips;

    PipGrid () {
        pips = new int[WIDTH][HEIGHT];
    }

    public int getPip (Position position) {
        return pips[position.getX()][position.getY()];
    }

}
