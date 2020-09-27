import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Solver of the Domino problem
 */
public class Solver {

    /**
     * Grid of pips, input to solver
     */
    private PipGrid input;

    /**
     * List of found solutions
     */
    List solutions; // TODO not array (fixed size) but list

    /**
     * Creates a new solver, to solve at least one of your problems
     */
    public Solver() {
        this.solutions = new ArrayList<BoneGrid>();
    }

    /**
     * Solve the Domino problem for the given input
     * @param input to solve
     * @return
     */
    public List solve (PipGrid input) {
        this.input = input;
      BoneGrid currentBoard = new BoneGrid();
      List<Bone> availableBones = new BoneSet().returnBones();

        Grid.Position startingPosition = new Grid.Position(-1,-1);
      gotoNextPosition(startingPosition, currentBoard, availableBones);
    /* do something and solve it */
        // TODO how to now when ready
        return solutions ;
    }

    /**
     * Move solver to next position TODO
     * @param position
     * @param currentBoneGrid
     * @param availableBones
     * @return
     */
    private void gotoNextPosition(Grid.Position position, BoneGrid currentBoneGrid, List<Bone> availableBones){
        Grid.Position nextPostion = currentBoneGrid.nextEmpty(position);

        for (Bone bone : availableBones) {
            allOrientations(currentBoneGrid.copy(), nextPostion, bone, BoneSet.copyBoneList(availableBones));
            // TODO have to use new copies, instead of reference TODO copy/clone
        }
        // TODO returns?
    }

    private void allOrientations(BoneGrid boneGrid, Grid.Position position, Bone bone, List<Bone> availableBones){
        // always do horizontal and vertical
        checkAndPlace(boneGrid, position, position, Grid.Position.horizontal(position), bone, availableBones );
        checkAndPlace(boneGrid,  position,position, Grid.Position.vertical(position), bone, availableBones );

        if (!bone.isSymmetrical()) {
            // do invHorizontal and invVertical
            checkAndPlace(boneGrid,  position,Grid.Position.horizontal(position), position, bone, availableBones );
            checkAndPlace(boneGrid,  position,Grid.Position.vertical(position), position, bone, availableBones );

        }
    }

    private void checkAndPlace(BoneGrid boneGrid, Grid.Position currentPosition, Grid.Position position1, Grid.Position position2, Bone bone, List<Bone> availableBones){
        if (availableBones.size() == 1) {
            if (valid(boneGrid,position1,position2,bone,availableBones)) {
                // place bone
                boneGrid.setBone(bone, position1);
                boneGrid.setBone(bone, position2);

                // add resulting BoneGrid to List
                this.solutions.add(boneGrid);
            } else {
                // do nothing because nonesense
            }
        } else {
            if (valid(boneGrid,position1,position2,bone,availableBones)) {
                // place bone
                boneGrid.setBone(bone, position1);
                boneGrid.setBone(bone, position2);
                availableBones.remove(bone);

                // and gotoNextPosition
                gotoNextPosition(currentPosition /* TODO */,boneGrid,availableBones);
            } else {
                // if invalid: do nothing because nonesense
            }
        }
    }

    private boolean checkOrientation() {
        return false;
    }


    // Validation of placing bone on positions
    private boolean valid(BoneGrid boneGrid, Grid.Position position1, Grid.Position position2,
                          Bone bone, List<Bone> availableBones) {
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

    private boolean validNotUsed (Bone bone, List<Bone> availableBones) {
        return availableBones.contains(bone);
        // TODO: use a List or stream?
    }
}
