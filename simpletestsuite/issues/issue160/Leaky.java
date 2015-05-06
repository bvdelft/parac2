public class Leaky
{
  public static final java.lang.Object observer = new java.lang.Object();
  public static final se.chalmers.paragon.Policy low = se.chalmers.paragon.Policy.newPolicy("low", se.chalmers.paragon.Policy.newPClause(observer));
  public static final se.chalmers.paragon.Policy high = se.chalmers.paragon.Policy.newPolicy("high");
  boolean lowData;
  boolean highData;
  public void leakStuff(boolean b) throws java.lang.Exception
  {
    if (b)
    throw new java.lang.Exception();
  }
  public void abuseLeak()
  {
    try
    {
      leakStuff(highData);
      lowData = false;
    }
    catch (java.lang.Exception e)
    {
      lowData = true;
    }
  }
}