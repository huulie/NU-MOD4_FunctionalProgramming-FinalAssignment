import java.util.ArrayList;
import java.util.List;

public class BoneSet {

   List<Bone> bones;

    /**
     * Generates a Double Six bone set
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

    /**
     * Returns the Double Six bone set as array of bones
     * @return array of bones
     */
   public List<Bone> returnBones () {
       return bones;
   }

   public static List<Bone> copyBoneList(List<Bone> original) {
       List<Bone> copiedList = new ArrayList<Bone>();
       copiedList.addAll(original);
       return copiedList;
   }

}
