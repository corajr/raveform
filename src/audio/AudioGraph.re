open Audio;

type nodeId = string;

/* source, target, source output index, target input index */
type edge = (nodeId, nodeId, int, int);

module EdgeComparator =
  Belt.Id.MakeComparable({
    type t = edge;
    let cmp = ((a0, a1, a2, a3), (b0, b1, b2, b3)) =>
      switch (Pervasives.compare(a0, b0)) {
      | 0 =>
        switch (Pervasives.compare(a1, b1)) {
        | 0 =>
          switch (Pervasives.compare(a2, b2)) {
          | 0 => Pervasives.compare(a3, b3)
          | c => c
          }
        | c => c
        }
      | c => c
      };
  });

type edgeSet = Belt.Set.t(EdgeComparator.t, EdgeComparator.identity);

let emptyEdgeSet = Belt.Set.make(~id=(module EdgeComparator));

type audioGraph = {
  /* map from string node ID to audioNode instance */
  nodes: Belt.Map.String.t(audioNode),
  /* set of edges */
  edges: edgeSet,
  /* all edges actually connected in WebAudio's graph */
  actuallyConnectedEdges: edgeSet,
};

let emptyAudioGraph: audioGraph = {
  nodes: Belt.Map.String.empty,
  edges: emptyEdgeSet,
  actuallyConnectedEdges: emptyEdgeSet,
};

let getNode = (key: nodeId, graph: audioGraph) : option(audioNode) =>
  Belt.Map.String.get(graph.nodes, key);

let addNode =
    ((key, node): (nodeId, audioNode), graph: audioGraph)
    : audioGraph => {
  ...graph,
  nodes: Belt.Map.String.set(graph.nodes, key, node),
};

let partitionEdgesWithNode =
    (key: nodeId, edgeSet: edgeSet)
    : (edgeSet, edgeSet) =>
  Belt.Set.partition(edgeSet, ((sourceId, targetId, _, _)) =>
    key == sourceId || key == targetId
  );

let removeAllEdgesInvolvingNode = (key: nodeId, graph: audioGraph) => {
  ...graph,
  edges: snd(partitionEdgesWithNode(key, graph.edges)),
};

let removeNode = (key: nodeId, graph: audioGraph) : audioGraph => {
  ...graph,
  nodes: Belt.Map.String.remove(graph.nodes, key),
};

let addEdge = (edge: edge, graph: audioGraph) : audioGraph => {
  ...graph,
  edges: Belt.Set.add(graph.edges, edge),
};

let removeEdge = (edge: edge, graph: audioGraph) : audioGraph => {
  ...graph,
  edges: Belt.Set.remove(graph.edges, edge),
};

let maybeApplyToGraph =
    (
      f: (audioNode, audioNode, int, int) => unit,
      (sourceId, targetId, outputIndex, inputIndex): edge,
      graph: audioGraph,
    )
    : option(edge) => {
  let maybeSource = Belt.Map.String.get(graph.nodes, sourceId);
  let maybeTarget = Belt.Map.String.get(graph.nodes, targetId);

  switch (maybeSource, maybeTarget) {
  | (Some(source), Some(target)) =>
    f(source, target, outputIndex, inputIndex);
    Some((sourceId, targetId, outputIndex, inputIndex));
  | _ => None
  };
};

let disconnectEdges = (edgesToDisconnect: edgeSet, graph) =>
  Belt.Set.forEach(
    edgesToDisconnect,
    edge => {
      maybeApplyToGraph(disconnectWithOutputAndInputIndex, edge, graph);
      ();
    },
  );

let updateConnections = (graph: audioGraph) : audioGraph => {
  let edgesToConnect =
    Belt.Set.diff(graph.edges, graph.actuallyConnectedEdges);

  let edgesToDisconnect =
    Belt.Set.diff(graph.actuallyConnectedEdges, graph.edges);

  disconnectEdges(edgesToDisconnect, graph);

  let nowConnected =
    Belt.Set.reduce(edgesToConnect, emptyEdgeSet, (acc, edge) =>
      switch (maybeApplyToGraph(connectWithOutputAndInputIndex, edge, graph)) {
      | Some(edge) => Belt.Set.add(acc, edge)
      | None => acc
      }
    );

  {...graph, actuallyConnectedEdges: nowConnected};
};

let replaceNode =
    (key: nodeId, newNode: audioNode, graph: audioGraph)
    : audioGraph => {
  let (actualEdgesWithNode, actualEdgesWithoutNode) =
    partitionEdgesWithNode(key, graph.actuallyConnectedEdges);

  disconnectEdges(actualEdgesWithNode, graph);

  let newNodes = Belt.Map.String.set(graph.nodes, key, newNode);
  {...graph, nodes: newNodes, actuallyConnectedEdges: actualEdgesWithoutNode};
};
