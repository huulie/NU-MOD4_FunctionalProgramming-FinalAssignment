import javax.swing.text.Position;

public class Solver {

    /**
     * List of bones used in the current solution (PM: one array per solution)
     */
    private Bone[][] usedBones;

    private BoneGrid[] solutions;



    private Grid[] solve (Grid input ) {
BoneGrid currentSolution;
//        -- CheckOrientaties voor positie (x,y)
//        -- Is ((x,y) , (x+1,y)) een valide mogelijkheid?
//                --   Zoja: CheckOrientaties voor de volgende positie
//        --   Zonee: = oplossing|iets doms
//                -- Is ((x,y), (x,y+1)) een valide mogelijkheid?
//                --   Zoja: CheckOrientaties voor de volgende positie
//        --   Zonee:  = oplossing|iets doms
//                -- !! PM: orientation can also be "reversed" horizontal/vertical, because not all bones are symmetric
//                -- misschien met een variable bord die in recursie steeds kleiner wordt als posities gevuld? Dan ook dat probleem ondervangen

        return solutions ;
    }

    private boolean checkOrientation() {
        return false;
    }

    private boolean valid() {
        return false;
    }

    private boolean validOnBoard (BoneGrid grid, Grid.Position position){
       return grid.isOnBoard(position);
    }


}
