public class MemberPolicyLeak
{
  public static int globalLeak = 0;
  public int dataInSecObj;
  public static final se.chalmers.paragon.Policy pub = se.chalmers.paragon.Policy.newPolicy("pub", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  public static final se.chalmers.paragon.Policy sec = se.chalmers.paragon.Policy.newPolicy("sec");
  public MemberPolicyLeak (int data)
  {
    this.dataInSecObj = data;
  }
  public void doLeak()
  {
    globalLeak = dataInSecObj;
  }
  public int getDataInSecObj()
  {
    return dataInSecObj;
  }
  public static boolean leakit(boolean secretBool)
  {
    MemberPolicyLeak leakerA = new MemberPolicyLeak(1);
    MemberPolicyLeak leakerB = new MemberPolicyLeak(2);
    MemberPolicyLeak leaker = leakerB;
    if (secretBool)
    leaker = leakerA;
    leaker.doLeak();
    int leakedData = globalLeak;
    return leakedData == 1;
  }
}