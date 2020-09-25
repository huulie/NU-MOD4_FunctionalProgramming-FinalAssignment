public class BoneGrid extends Grid {

    /**
     * Grid with numbers of placed bones (output of the solver)
     */
    private Bone[][] bones;

    BoneGrid () {
        bones = new Bone[WIDTH][HEIGHT];
    }

    private Bone getBone (Position position) {
        return bones[position.getX()][position.getY()];
    }

    private void setBone (Bone bone, Position position) {
        bones[position.getX()][position.getY()] = bone;
    }

    private BoneGrid deepCopy() throws CloneNotSupportedException {
        return (BoneGrid) this.clone(); // TODO casting?
        // TODO no problem if Bone objects are existing references, and no new objects
    }
}
