import {DateTimeFormatter} from "@js-joda/core";

export function formatISODate(date) {
  return date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
}

export function formatISODateTime(dateTime) {
    return dateTime.format(DateTimeFormatter.ofPattern('yyyy-MM-dd HH:mm'))
}
