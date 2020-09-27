public class BoneGrid extends Grid {

    /**
     * Grid with numbers of placed bones (output of the solver)
     */
    private Bone[][] bones;


    BoneGrid () {
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
            return new Position(0,0);
        }

        Position currentPosition = position;

        while (this.getBone(currentPosition) != null) { // TODO infinite loop protection?
            currentPosition = Position.next(currentPosition);
            if(!isOnBoard(currentPosition)) {
                System.out.println("Searching next free got out of bounds, even with next starting at origin again"); // TODO
                return null;
            }
        }
        return currentPosition; // TODO right to return?
    }

    public BoneGrid copy() {
        BoneGrid copiedGrid = new BoneGrid();
//        try {
//            copiedGrid = (BoneGrid) this.clone(); // TODO casting?
//        } catch (CloneNotSupportedException e) {
//            e.printStackTrace();
//            // TODO errorhandling
//        }
        for (int i = 0; i < HEIGHT*WIDTH; i++) {
            if (this.getBone(Position.index2position(i)) != null) { // if null, keep null
                copiedGrid.setBone(this.getBone(Position.index2position(i)), Position.index2position(i));
            }
        }
        // TODO no problem if Bone objects are existing references, and no new objects
        return copiedGrid;
    }
}
