package com.wage.test

import org.junit.Test

import com.wage.WageCalculator

class WageCalculatorTest {
  @Test def testConvertMinuteToHoursMinutes() = {
    val minutes = 493
    println("testConvertMinuteToHoursMinutes => " + minutes + " = " + WageCalculator.convertMinuteToHoursMinutes(minutes.toDouble) + " (HH:mm)")
  }
  
  @Test def testEveningShiftWorked() = {
    val eveningMinutes = WageCalculator.calculateEveningMinutes(WageCalculator.generateDate("02.12.2014 16:00"), WageCalculator.generateDate("02.12.2014 23:15"))
    println("testEveningShiftWorked => " + WageCalculator.convertMinuteToHoursMinutes(eveningMinutes.toDouble) + " (HH:mm) can be classified as Evening Shift")
  }

  @Test def testGenerateDate() = {
    val stringDate = "02.12.2014 23:15"
    println("testGenerateDate => " + WageCalculator.generateDate(stringDate) + " *** is Joda based DateTime")
  }

  @Test def testRoundPayToNearestCent() = {
    val calculatedPay = 533.78842
    println("testRoundPayToNearestCent => " + WageCalculator.roundToNearestCent(calculatedPay) + " Pay Rounded to nearest cent from: " + calculatedPay)
  }

  @Test def testRoundAtAnyDecimalPoint() = {
    val calculatePayAtDifferentDecimalPoints = 533.875969
    println("testRoundAtAnyDecimalPoint => Original=" +calculatePayAtDifferentDecimalPoints+ " - Rounded to 3 Decimal Points: " 
        +WageCalculator.roundAt(calculatePayAtDifferentDecimalPoints,3) + " --- Rounded to 5 Decimal Points: " + WageCalculator.roundAt(calculatePayAtDifferentDecimalPoints,5))
  }

  @Test def testGetTotalInterval() = {
    val totalInterval = WageCalculator.getTotalInterval(WageCalculator.generateDate("02.12.2014 16:00"), WageCalculator.generateDate("02.12.2014 23:15"))
    println("testGetTotalInterval => " + "Read after PT  -- " + totalInterval.toPeriod() )
  }
}