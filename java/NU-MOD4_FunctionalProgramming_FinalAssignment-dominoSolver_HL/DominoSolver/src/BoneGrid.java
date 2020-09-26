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

    public Position nextEmpty(Position position) {
        Position currentPosition = position;

        if (Position.next(currentPosition) == null) {
            return Position.next(currentPosition);
        } else {
            return nextEmpty(Position.next(currentPosition)); // TODO right to return?
        }
    }

    public BoneGrid copy() {
        BoneGrid copiedGrid = null;
        try {
            copiedGrid = (BoneGrid) this.clone(); // TODO casting?
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            // TODO errorhandling
        }
        // TODO no problem if Bone objects are existing references, and no new objects
        return copiedGrid;
    }
}
