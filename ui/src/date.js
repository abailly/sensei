function formatTo2Digits(value) {
    if (value < 10) {
        return  '0' + value;
    }
    return value;
}

export function formatISODate(date) {
    const year = date.getFullYear();
    let month = date.getMonth() + 1;
    let dt = date.getDate();
    return year + '-' + formatTo2Digits(month) + '-' + formatTo2Digits(dt);
}

export function formatISOTime(date) {
    let hours = date.getHours();
    let minutes = date.getMinutes();
    return formatTo2Digits(hours) + ':' + formatTo2Digits(minutes);
}
