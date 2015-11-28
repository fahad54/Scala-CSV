package com.wage

import scala.collection.mutable.HashMap
import scala.io.Source

import org.joda.time.Interval

import com.github.nscala_time.time.Imports.{DateTime, DateTimeFormat, richDateTime, richInt, richReadableInstant, richReadableInterval}

object WageCalculator extends App {
  var employeeRecord: HashMap[String, Array[Long]] = new HashMap
  var tempPersonName: String = _
  var tempPersonId: String = _
  var tempDate: String = _
  var tempStart: String = _
  var tempEnd: String = _
  var tempTotalMinutes: Long = _
  var tempNormalMinutes: Long = _
  var tempEveningMinutes: Long = _
  var firstRecord: Boolean = true

  val dataInputFile = System.getProperty("user.dir") + "/HourList201403.csv"
  val completeData = Source.fromFile(dataInputFile).getLines().map(_.split(","))
  val dataHeader = completeData.next
  val contentIterator = completeData.map(dataHeader.zip(_).toMap)
  val NORMAL_RATE = 3.75
  val RATE_PER_MINUTE = NORMAL_RATE / 60
  val EVENING_RATE = RATE_PER_MINUTE + (1.15 / 60)
  val OVERTIME_RATE_0_2 = RATE_PER_MINUTE * 1.25
  val OVERTIME_RATE_2_4 = RATE_PER_MINUTE * 1.5
  val OVERTIME_RATE_4_16 = RATE_PER_MINUTE * 2
  
  while (contentIterator.hasNext) {
    contentIterator foreach (row => singleRecordProcessing(row("Person Name"), row("Person ID"), row("Date"), row("Start"), row("End")))
  }
  employeeRecord.keys.foreach(k => {
    print("Employee ID: " + k);
    {
      val singleEmployee = calculateWage(employeeRecord(k))
      print(", TotalPay: " + singleEmployee(0) + "$, Pay For Normal Hours: " + singleEmployee(1) + ", Pay For Evening Hours: " + singleEmployee(2) + ", Pay For Overtime Hours 0-2: "
        + singleEmployee(3) + ", Pay For Overtime Hours 2-4: " + singleEmployee(4) + ", Pay For Overtime Hours 4-16: " + singleEmployee(5))
    }
    println
  })

  def calculateWage(singleEmployee: Array[Long]) = {
    val normalPay = roundToNearestCent(singleEmployee(1) * RATE_PER_MINUTE)
    val eveningPay = roundToNearestCent(singleEmployee(2) * EVENING_RATE)
    val overtime0_2Pay = roundToNearestCent(singleEmployee(3) * OVERTIME_RATE_0_2)
    val overtime2_4Pay = roundToNearestCent(singleEmployee(4) * OVERTIME_RATE_2_4)
    val overtime4_16Pay = roundToNearestCent(singleEmployee(5) * OVERTIME_RATE_4_16)
    val totalPay = normalPay + eveningPay + overtime0_2Pay + overtime2_4Pay + overtime4_16Pay
    Array(roundToNearestCent(totalPay), normalPay, eveningPay, overtime0_2Pay, overtime2_4Pay, overtime4_16Pay)
  }

  def roundAt(number: Double, precision: Int): Double = { val s = math pow (10, precision); (math round number * s) / s }
  
  def roundToNearestCent(number: Double) = roundAt(number, 2)
  
  def singleRecordProcessing(personName: String, personId: String, date: String, start: String, end: String) = {
    val startDate = generateDate(date + " " + start)
    val endDate = correctedEndDate(startDate, generateDate(date + " " + end))
    val totalMinutes = getTotalInterval(startDate, endDate).duration.getStandardMinutes()
    val eveningMinutes = calculateEveningMinutes(startDate, endDate)
    val normalMinutes = totalMinutes - eveningMinutes //Implicit conversion found
    val currentValues = Array(totalMinutes, normalMinutes, eveningMinutes, 0L, 0L, 0L)
    (tempTotalMinutes > 0) match {
      case true => {
        (personId.equalsIgnoreCase(tempPersonId) && (date.equalsIgnoreCase(tempDate))) match {
          case true => addTempWithCurrentRecord(totalMinutes, normalMinutes, eveningMinutes)
          case false => updateEmployeeRecords(personId, checkOvertime(currentValues))
        }
      }
      case false => {
        assignTempValues(personName, personId, date, start, end, totalMinutes, normalMinutes, eveningMinutes)
        firstRecord match {
          case true => {
            updateEmployeeRecords(personId, checkOvertime(currentValues))
            firstRecord = false
          }
          case _ =>
        }
      }
    }
  }

  def checkOvertime(currentValues: Array[Long]) = {
    (currentValues.length > 5) match {
      case true => {
        (currentValues(0) > 480) match {
          case true => {
            (currentValues(0) <= 600) match {
              case true => { Array(currentValues(0), 480L, 0L, (currentValues(0) - 480L), 0L, 0L) }
              case false => {
                (currentValues(0) > 600 && currentValues(0) <= 720) match {
                  case true => { Array(currentValues(0), 480L, 0L, 0L, (currentValues(0) - 480L), 0L) }
                  case false => { Array(currentValues(0), 480L, 0L, 0L, 0L, (currentValues(0) - 480L)) }
                }
              }
            }
          }
          case false => currentValues
        }
      }
      case false => currentValues
    }
  }

  def generateDate(date: String) = {
    val dateTimeFormatter = DateTimeFormat.forPattern("dd.MM.yyyy HH:mm")
    dateTimeFormatter.parseDateTime(date)
  }

  def correctedEndDate(startDate: DateTime, endDate: DateTime) = {
    if (startDate > endDate) { 
      endDate + 1.day 
    } else endDate
  }

  def addTempWithCurrentRecord(totalMinutes: Long, normalMinutes: Long, eveningMinutes: Long) = {
    tempTotalMinutes += totalMinutes
    tempNormalMinutes += normalMinutes
    tempEveningMinutes += eveningMinutes
  }

  def getTotalInterval(startDate: DateTime, endDate: DateTime) = {
    (startDate > endDate) match {
      case true => new Interval(endDate, startDate)
      case _ => new Interval(startDate, endDate)
    }
  }

  def updateEmployeeRecords(employeeId: String, workHourValues: Array[Long]) {
    employeeRecord.contains(employeeId) match {
      case true => entryExists(employeeId, workHourValues)
      case false => addEmployee(employeeId, workHourValues)
    }
  }

  def entryExists(employeeId: String, workHourValues: Array[Long]) {
    val oldEntry = employeeRecord(employeeId)
    (oldEntry.length == workHourValues.length) match {
      case true => {
        for (entry <- 0 to oldEntry.length - 1) {
          oldEntry(entry) = oldEntry(entry) + workHourValues(entry)
        }
      }
      case false =>
    }
    employeeRecord.update(employeeId, oldEntry)
  }

  def addEmployee(employeeId: String, workHourValues: Array[Long]) {
    employeeRecord.put(employeeId, workHourValues)
  }

  def assignTempValues(personName: String, personId: String, date: String, start: String, end: String, totalMinutesLong: Long, normalMinutes: Long, eveningMinutes: Long) {
    tempPersonName = personName
    tempPersonId = personId
    tempDate = date
    tempStart = start
    tempEnd = end
    tempTotalMinutes = totalMinutesLong
    tempNormalMinutes = normalMinutes
    tempEveningMinutes = eveningMinutes
  }

  def calculateEveningMinutes(startDate: DateTime, endDate: DateTime) = {
    val eveningStart = startDate.getDayOfMonth().toString + "." + startDate.getMonthOfYear().toString + "." + startDate.getYear().toString
    val eveningEnd = (startDate.getDayOfMonth() + 1).toString + "." + startDate.getMonthOfYear().toString + "." + startDate.getYear().toString
    val eveningStartTime = generateDate(eveningStart + " " + "18:00")
    val eveningEndTime = generateDate(eveningEnd + " " + "05:59")
    val eveningShift = new Interval(eveningStartTime, eveningEndTime)
    val workedShift = new Interval(startDate, endDate)
    if (workedShift.overlaps(eveningShift)) {
      val workedEveningShift = workedShift.overlap(eveningShift)
      val workedEveningShiftMinutes = workedEveningShift.duration.getStandardMinutes()
      workedEveningShiftMinutes
    } else {
      0L
    }
  }
  
  def convertMinuteToHoursMinutes(minutes: Double): String = {
    val minutesInInt = minutes.toInt
    val hours: Int = minutesInInt / 60
    val remainingMinutes: Int = minutesInInt % 60
    val totalHours = hours.toString + ":" + remainingMinutes.toString
    totalHours
  }
}