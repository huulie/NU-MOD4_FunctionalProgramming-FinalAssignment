public class Bone {

    /**
     * Number of the bone
     */
    private int number;

    /**
     * Number of pips on the left side
     */
    private int pipsLeft;

    /**
     * Number of pips on the right side
     */
    private int pipsRight;

    public Bone (int number, int pipsLeft, int pipsRight) {
        this.number = number;
        this.pipsLeft = pipsLeft;
        this.pipsRight = pipsRight;
    }

    public int getNumber() {
        return number;
    }

    public int getPipsLeft() {
        return pipsLeft;
    }

    public int getPipsRight() {
        return pipsRight;
    }

    public boolean isSymmetrical () {
        return this.getPipsLeft() == this.getPipsRight();
    }

}
