package solver;

import domino.Bone;
import domino.Grid;

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


    // TODO move to superclass Grid?!
    public Position nextEmptyPosition(Position position) {
        if(position.getX() == -1 && position.getY() == -1) {
            return new Position(0,0); // starting position
        }

        Position currentPosition = position;

        while (this.getElementAt(currentPosition) != null) {
            currentPosition = Position.next(currentPosition);
            if(!isOnBoard(currentPosition)) {
                return null; // should never happen, because next starts at origin again
            }
        }
        return currentPosition;
    }

    @Override
    public BoneGrid copy() {
        BoneGrid copiedGrid = new BoneGrid();

        // TODO: move this to superclass?!
        for (int i = 0; i < HEIGHT*WIDTH; i++) {
            if (this.getElementAt(Position.index2position(i)) != null) { // if null, keep null
                copiedGrid.setElementAt(this.getElementAt(Position.index2position(i)), Position.index2position(i));
            }
        }

        return copiedGrid;
    }
}
