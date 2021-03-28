import { config } from "./config";

const white = "#ffffff";

export function colorOf(viewType) {

  if (config.userProfile) {
    return config.userProfile.userFlowTypes[viewType] ?? white;
  }

  switch (viewType) {
    case 'Learning':
      return "#ff8822";
    case 'Experimenting':
      return "#0022dd";
    case 'Troubleshooting':
      return "#ee1111";
    case 'Flowing':
      return "#00dd22";
    case 'Rework':
      return "#4500dd";
    case 'Note':
      return "#000000";
    case 'Meeting':
      return "#fff203";
    default:
      return "#ffffff";
  }
}
