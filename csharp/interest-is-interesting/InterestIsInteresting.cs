using System;

static class SavingsAccount
{
    public static float InterestRate(decimal balance)
    {
        float interest = 0.5F;
        if (balance >= 5000) {
            interest = 2.475F;
        } else if (balance >= 1000) {
            interest = 1.621F;
        } else if (balance < 0) {
            interest = -3.213F;
        }
        return interest;
    }

    public static decimal AnnualBalanceUpdate(decimal balance)
    {
        return (balance * (decimal)(Math.Abs(InterestRate(balance) / 100))) + balance;
    }

    public static int YearsBeforeDesiredBalance(decimal balance, decimal targetBalance)
    {
        int years = 0;
        while (balance < targetBalance) {
            years += 1;
            balance = AnnualBalanceUpdate(balance);
        }
        return years;
    }
}
