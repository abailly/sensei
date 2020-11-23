import { config } from "./config";

const white = "#ffffff";

export function colorOf(flowType) {

  if (config.userProfile) {
    return config.userProfile.userFlowTypes[flowType] ?? white;
  }

  switch (flowType) {
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
