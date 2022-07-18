// modified from https://bl.ocks.org/d3noob/43a860bc0024792f8803bba8ca0d5ecd
svg.selectAll('g').remove();

var treeData = data;

var margin = { top: 10, right: 40, bottom: 10, left: 40 },
  width = width - margin.left - margin.right,
  height = height - margin.top - margin.bottom;

// append the svg object to the body of the page
// appends a 'group' element to 'svg'
// moves the 'group' element to the top left margin
var svg = svg
  .append('svg')
  .attr('width', width + margin.right + margin.left)
  .attr('height', height + margin.top + margin.bottom)
  .append('g')
  .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

if (!data) {
  svg
    .append('text')
    .attr('x', width / 2)
    .attr('y', height / 2)
    .attr('text-anchor', 'middle')
    .style('font-size', '1em')
    .style('fill', '#E53935')
    .text('It seems like you do not have uploaded files !!!');
} else {
  var i = 0,
    duration = 600,
    linkScale = 0.5,
    nodeDistanceScale = 1.8,
    root;

  // declares a tree layout and assigns the size
  var treemap = d3.tree().size([height / nodeDistanceScale, width]);

  // Assigns parent, children, height, depth
  root = d3.hierarchy(treeData, function (d) {
    return d.children;
  });
  root.x0 = height / 2;
  root.y0 = 0;

  // Collapse after the second level
  root.children.forEach(collapse);

  update(root);

  // Collapse the node and all it's children
  function collapse(d) {
    if (d.children) {
      d._children = d.children;
      d._children.forEach(collapse);
      d.children = null;
    }
  }

  function update(source) {
    // Assigns the x and y position for the nodes
    var treeData = treemap(root);

    // Compute the new tree layout.
    var nodes = treeData.descendants(),
      links = treeData.descendants().slice(1);

    // Normalize for fixed-depth.
    nodes.forEach(function (d) {
      d.y = d.depth * 180;
    });

    // ****************** Nodes section ***************************

    // Update the nodes...
    var node = svg.selectAll('g.node').data(nodes, function (d) {
      return d.id || (d.id = ++i);
    });

    // Enter any new nodes at the parent's previous position.
    var nodeEnter = node
      .enter()
      .append('g')
      .attr('class', 'node')
      .attr('transform', function (d) {
        return (
          'translate(' +
          source.y0 * linkScale +
          ',' +
          source.x0 * nodeDistanceScale +
          ')'
        );
      })
      .on('click', click)
      .on('mouseover', function (d) {
        d3.select(this)
          .select('text')
          .transition()
          .duration(duration)
          .ease(d3.easeLinear)
          .style('opacity', 1);
      })
      .on('mouseout', function (d) {
        d3.select(this)
          .select('text')
          .transition()
          .duration(duration)
          .ease(d3.easeLinear)
          .style('opacity', function (d) {
            return d.data.font_opacity;
          });
      });

    // Add Circle for the nodes
    nodeEnter
      .append('circle')
      .attr('class', 'node')
      .attr('r', 1e-6)
      .style('fill', function (d) {
        return d.data.node_color;
      });

    // Add labels for the nodes
    nodeEnter
      .append('text')
      .attr('dy', '.35em')
      .attr('y', -15)
      .attr('text-anchor', 'middle')
      .text(function (d) {
        return d.data.name;
      })
      .style('font', '0.8em sans-serif')
      .style('opacity', function (d) {
        return d.data.font_opacity;
      })
      .style('fill', function (d) {
        return d.data.node_color;
      });

    // UPDATE
    var nodeUpdate = nodeEnter.merge(node);

    // Transition to the proper position for the node
    nodeUpdate
      .transition()
      .duration(duration)
      .attr('transform', function (d) {
        return (
          'translate(' + d.y * linkScale + ',' + d.x * nodeDistanceScale + ')'
        );
      });

    // Update the node attributes and style
    nodeUpdate
      .select('circle.node')
      .attr('r', 6)
      .style('fill', '#fff')
      .style('stroke', function (d) {
        return d.data.node_color;
      })
      .style('stroke-width', '5px')
      .attr('cursor', 'pointer');

    // Remove any exiting nodes
    var nodeExit = node
      .exit()
      .transition()
      .duration(duration)
      .attr('transform', function (d) {
        return (
          'translate(' +
          source.y * linkScale +
          ',' +
          source.x * nodeDistanceScale +
          ')'
        );
      })
      .remove();

    // On exit reduce the node circles size to 0
    nodeExit.select('circle').attr('r', 1e-6);

    // On exit reduce the opacity of text labels
    nodeExit.select('text').style('fill-opacity', 1e-6);

    // ****************** links section ***************************

    // Update the links...
    var link = svg.selectAll('path.link').data(links, function (d) {
      return d.id;
    });

    // Enter any new links at the parent's previous position.
    var linkEnter = link
      .enter()
      .insert('path', 'g')
      .attr('class', 'link')
      .style('fill', 'none')
      .style('stroke-width', '2.5px')
      .style('stroke', function (d) {
        return d.data.node_color;
      })
      .attr('d', function (d) {
        var o = { x: source.x0, y: source.y0 };
        return diagonal(o, o);
      });

    // UPDATE
    var linkUpdate = linkEnter.merge(link);

    // Transition back to the parent element position
    linkUpdate
      .transition()
      .duration(duration)
      .attr('d', function (d) {
        return diagonal(d, d.parent);
      });

    // Remove any exiting links
    var linkExit = link
      .exit()
      .transition()
      .duration(duration)
      .attr('d', function (d) {
        var o = { x: source.x, y: source.y };
        return diagonal(o, o);
      })
      .remove();

    // Store the old positions for transition.
    nodes.forEach(function (d) {
      d.x0 = d.x;
      d.y0 = d.y;
    });

    // Creates a curved (diagonal) path from parent to the child nodes
    function diagonal(s, d) {
      path = `M ${s.y * linkScale} ${s.x * nodeDistanceScale}
              C ${((s.y + d.y) / 2) * linkScale} ${s.x * nodeDistanceScale},
                ${((s.y + d.y) / 2) * linkScale} ${d.x * nodeDistanceScale},
                ${d.y * linkScale} ${d.x * nodeDistanceScale}`;
      return path;
    }

    // Toggle children on click.
    function click(d) {
      if (d.children) {
        d._children = d.children;
        d.children = null;
      } else {
        d.children = d._children;
        d._children = null;
      }
      update(d);
    }
  }
}
