/**
 * @fileOverview Record page for creating flow events via colorized buttons
 * @name record.js
 */

import { dom, clearElement } from './dom';
import { config } from './config';
import { post } from './request';

/**
 * Get the current timestamp in ISO format
 * @returns {string} ISO 8601 formatted timestamp
 */
function getCurrentTimestamp() {
  return new Date().toISOString();
}

/**
 * Record a flow event of the given type
 * @param {object} router - Navigo router instance
 * @param {string} flowType - The flow type to record
 * @param {function} onSuccess - Callback on successful recording
 * @param {function} onError - Callback on error
 */
function recordFlow(router, flowType, onSuccess, onError) {
  const userName = config.userProfile.userName;
  const timestamp = getCurrentTimestamp();
  // Use a placeholder directory since we're in the web UI
  const flowDir = '/web';

  const event = {
    tag: 'Flow',
    version: 13,
    flowType: flowType,
    flowUser: userName,
    flowTimestamp: timestamp,
    flowDir: flowDir
  };

  post(router, `/api/log/${userName}`, [event], onSuccess, onError);
}

/**
 * Create a status message element
 * @param {string} message - The message to display
 * @param {string} type - 'success' or 'error'
 * @returns {HTMLElement}
 */
function createStatusMessage(message, type) {
  const className = type === 'success' ? 'record-status-success' : 'record-status-error';
  return <div class={className}>{message}</div>;
}

/**
 * Render the Record page with flow type buttons
 * @param {object} router - Navigo router instance
 * @param {HTMLElement} container - Container element to render into
 */
export function record(router, container) {
  clearElement(container);

  const flowTypes = config.userProfile.userFlowTypes || {};
  flowTypes['End'] = '#333333'; // Always include 'End' flow type
  const flowTypeEntries = Object.entries(flowTypes);

  // Status message container
  const statusContainer = <div class="record-status"></div>;

  // Create buttons for each flow type
  const buttons = flowTypeEntries.map(([flowType, color]) => {
    const button = <button
      class="record-button"
      style={`background-color: ${color};`}
      onclick={() => {
        // Disable button during request
        button.disabled = true;
        button.classList.add('record-button-loading');

        recordFlow(router, flowType,
          () => {
            // Success
            button.disabled = false;
            button.classList.remove('record-button-loading');
            clearElement(statusContainer);
            statusContainer.appendChild(
              createStatusMessage(`Recorded: ${flowType}`, 'success')
            );
            // Auto-hide after 3 seconds
            setTimeout(() => clearElement(statusContainer), 3000);
          },
          (error) => {
            // Error
            button.disabled = false;
            button.classList.remove('record-button-loading');
            clearElement(statusContainer);
            statusContainer.appendChild(
              createStatusMessage(`Error: ${error}`, 'error')
            );
          }
        );
      }}
    >
      {flowType}
    </button>;
    return button;
  });

  // Build the page content
  const content = <div class="content record-page">
    <h2>Record Flow</h2>
    <p class="record-description">
      Select a flow type to record. This will start tracking time for the selected activity.
    </p>
    {statusContainer}
    <div class="record-buttons">
      {buttons.length > 0 ? buttons : <p>No flow types configured. Please update your user profile.</p>}
    </div>
  </div>;

  container.appendChild(content);
  return content;
}

export default record;
