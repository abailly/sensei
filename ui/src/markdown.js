import showdown from 'showdown';

const showdownOptions = { simplifiedAutoLink: true, tables: true, emoji: true };

export default function markdownNote(note, timestamp) {
  const timeHeader = timestamp ?
    '#### ' + new Date(note.noteStart).toLocaleTimeString() + '\n\n' : '';

  const converter = new showdown.Converter(showdownOptions);
  converter.setFlavor('github');

  return converter.makeHtml(timeHeader + note);
}
