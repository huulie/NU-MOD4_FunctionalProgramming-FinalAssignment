package domino;

import java.util.ArrayList;
import java.util.List;

public class BoneSet {

   private List<Bone> bones;

    /**
     * Generates a Double Six bone set
     * (could be generalised to generate other variants, but YAGNI for now)
     */
   public BoneSet() {
       bones = new ArrayList<Bone>();

       int n = 1;
       for (int l = 0; l < 7; l++) {
           for (int r = l ; r < 7; r++) {
               bones.add(n-1, new Bone(n, l, r));
                       n++;
           }
       }
   }

   public List<Bone> returnBones () {
       return bones;
   }

    /**
     * Creates a new List of Bones, identical to the original board.Bone List.
     * @param original to copy
     * @return a copy of the original
     */
   public static List<Bone> copyBoneList(List<Bone> original) {
       List<Bone> copiedList = new ArrayList<Bone>(original);
       return copiedList;
   }
}
