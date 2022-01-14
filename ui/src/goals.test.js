import { computeGoalsGraph } from './goals.js';
import { describe, expect, test } from '@jest/globals';
import { LocalDate, LocalDateTime } from "@js-joda/core";

describe('Goals graph', () => {

  test('done nodes are colored in green', () => {
    const graph = {
      goalsGraph: [["foo", []], ["bar", []]],
      current: [],
      completed: ["bar"]
    };

    const result = computeGoalsGraph(graph);

    expect(result.nodes.find((n) => n.label === "bar").color).toBe('green');
  });

});
