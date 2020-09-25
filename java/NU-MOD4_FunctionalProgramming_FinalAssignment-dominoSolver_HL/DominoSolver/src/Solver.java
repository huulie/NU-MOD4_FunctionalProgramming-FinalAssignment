import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Solver {

    /**
     * Grid of pips, input to solver
     */
    private PipGrid input;

//    /** // TODO at top or in method?
//     * List of bones used in the current solution (PM: one array per solution)
//     */
//    private Bone[][] availableBones;

    /**
     * List of found solutions
     */
    List solutions; // TODO not array (fixed size) but list

    public Solver(PipGrid input) {
        this.input = input;
        this.solutions = new ArrayList<BoneGrid>();
    }

    private List solve (PipGrid input ) {
      BoneGrid currentBoard = new BoneGrid();
      Bone[] availableBones = new BoneSet().returnBones();

    /* do something and solve it */
        return solutions ;
    }

    private BoneGrid gotoNextPosition(/*..*/){

    }

    private BoneGrid allOrientations(/*..*/){

    }

    private BoneGrid checkAndPlace(/*..*/){
        if (availableBones.length == 1) {
            if (/* valid placement */) {
                // do place
                // add resulting BoneGrid to List
            } else {
                // do nothing because nonesense
            }
        } else {
            // if valid: gotoNextPosition
            // if invalid: do nothing because nonesense
        }
    }

    private boolean checkOrientation() {
        return false;
    }


    // Validation of placing bone on positions
    private boolean valid(BoneGrid boneGrid, Grid.Position position1, Grid.Position position2,
                          Bone bone, Bone[] availableBones) {
        return validOnBoard(boneGrid, position1) && validOnBoard(boneGrid, position2) &&
                validFree(boneGrid, position1) && validFree(boneGrid, position2) &&
                validPipMatch(position1, position2, bone) && validNotUsed(bone, availableBones);
    }

    private boolean validOnBoard (BoneGrid grid, Grid.Position position) {
       return grid.isOnBoard(position);
    }

    private boolean validFree (BoneGrid grid, Grid.Position position) {
        return grid.isFree(position);
    }

    private boolean validPipMatch (Grid.Position position1, Grid.Position position2, Bone bone) {
        return input.getPip(position1) == bone.getPipsLeft() && input.getPip(position2) == bone.getPipsRight();
    }

    private boolean validNotUsed (Bone bone, Bone[] availableBones) {
        return Arrays.asList(availableBones).contains(bone);
        // TODO: use a List or stream?
    }
}
