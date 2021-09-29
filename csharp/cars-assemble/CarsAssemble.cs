using System;

static class AssemblyLine
{
    public static double ProductionRatePerHour(int speed)
    {
        double production = speed * 221;
        if (speed == 10) {
            production *= 0.77;
        } else if (speed == 9) {
            production *= 0.8;
        } else if (speed > 4) {
            production *= 0.9;
        }
        return production;
    }

    public static int WorkingItemsPerMinute(int speed)
    {
        return (int)Math.Floor(ProductionRatePerHour(speed) / 60.0);
    }
}
