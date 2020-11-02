package board;

public class BoneGrid extends Grid {

    /**
     * board.Grid with numbers of placed bones (output of the solver)
     */
    private Bone[][] bones;


    public BoneGrid () { // TODO check modifier
        bones = new Bone[WIDTH][HEIGHT];
    }

    public Bone getBone(Position position) {
        return bones[position.getX()][position.getY()];
    }

    public void setBone(Bone bone, Position position) {
        bones[position.getX()][position.getY()] = bone;
    }

    @Override
    public boolean isFree(Position position) {
        return this.getBone(position) == null;
    }

    public Position nextEmpty(Position position) {
        if(position.getX() == -1 && position.getY() == -1) {
            return new Position(0,0); // starting position
        }

        Position currentPosition = position;

        while (this.getBone(currentPosition) != null) {
            currentPosition = Position.next(currentPosition);
            if(!isOnBoard(currentPosition)) {
                return null; // should never happen, because next starts at origin again
            }
        }
        return currentPosition;
    }

    public BoneGrid copy() {
        BoneGrid copiedGrid = new BoneGrid();

        for (int i = 0; i < HEIGHT*WIDTH; i++) {
            if (this.getBone(Position.index2position(i)) != null) { // if null, keep null
                copiedGrid.setBone(this.getBone(Position.index2position(i)), Position.index2position(i));
            }
        }

        return copiedGrid;
    }
}
