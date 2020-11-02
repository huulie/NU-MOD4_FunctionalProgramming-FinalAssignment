package domino;

/**
 * board.Grid with placed bones (output of the solver)
 */
public class BoneGrid extends Grid<Bone> {

    public BoneGrid () {
        super(new Bone[WIDTH][HEIGHT]);
    }

    @Override
    public Bone getElementAt(Position position) {
        return super.getElementAt(position);
    }

   @Override
    public void setElementAt(Bone bone, Position position) {
        super.setElementAt(bone, position);
    }

    /**
     * Creates a new BoneGrid, identical to the original BoneGrid.
     * @return a copy of this BoneGrid
     */
    public BoneGrid deepCopy() {
        BoneGrid copiedGrid = new BoneGrid();

        for (int i = 0; i < HEIGHT*WIDTH; i++) {
            if (this.getElementAt(Position.index2position(i)) != null) { // if null, keep null
                copiedGrid.setElementAt(this.getElementAt(Position.index2position(i)), Position.index2position(i));
            }
        }

        return copiedGrid;
    }
}
