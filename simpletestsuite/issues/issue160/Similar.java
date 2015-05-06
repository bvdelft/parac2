public class Similar
{
  boolean high = true;
  boolean low = true;
  public void m() throws java.lang.Exception
  {
    throw new java.lang.Exception();
  }
  public void n()
  {
    try
    {
      if (high)
      m();
      low = false;
    }
    catch (java.lang.Exception e)
    {
    }
  }
}