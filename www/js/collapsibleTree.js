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

if (!treeData) {
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
  // Expand the first node by default
  expand(root.children[0]);

  update(root);

  // Functions
  function expand(d) {
    var children = d.children ? d.children : d._children;
    if (d._children) {
      d.children = d._children;
      d._children = null;
    }
    if (children) children.forEach(expand);
  }

  // function expandAll() {
  //   expand(root);
  //   update(root);
  // }

  // function collapseAll() {
  //   root.children.forEach(collapse);
  //   collapse(root);
  //   update(root);
  // }

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
    // TODO: use font awesome, which has more icons, but
    // haven't figured out how to change size of appended path
    var icons = {
      geo: 'M4 4a4 4 0 1 1 4.5 3.969V13.5a.5.5 0 0 1-1 0V7.97A4 4 0 0 1 4 3.999zm2.493 8.574a.5.5 0 0 1-.411.575c-.712.118-1.28.295-1.655.493a1.319 1.319 0 0 0-.37.265.301.301 0 0 0-.057.09V14l.002.008a.147.147 0 0 0 .016.033.617.617 0 0 0 .145.15c.165.13.435.27.813.395.751.25 1.82.414 3.024.414s2.273-.163 3.024-.414c.378-.126.648-.265.813-.395a.619.619 0 0 0 .146-.15.148.148 0 0 0 .015-.033L12 14v-.004a.301.301 0 0 0-.057-.09 1.318 1.318 0 0 0-.37-.264c-.376-.198-.943-.375-1.655-.493a.5.5 0 1 1 .164-.986c.77.127 1.452.328 1.957.594C12.5 13 13 13.4 13 14c0 .426-.26.752-.544.977-.29.228-.68.413-1.116.558-.878.293-2.059.465-3.34.465-1.281 0-2.462-.172-3.34-.465-.436-.145-.826-.33-1.116-.558C3.26 14.752 3 14.426 3 14c0-.599.5-1 .961-1.243.505-.266 1.187-.467 1.957-.594a.5.5 0 0 1 .575.411z',
      table:
        'M0 2a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V2zm15 2h-4v3h4V4zm0 4h-4v3h4V8zm0 4h-4v3h3a1 1 0 0 0 1-1v-2zm-5 3v-3H6v3h4zm-5 0v-3H1v2a1 1 0 0 0 1 1h3zm-4-4h4V8H1v3zm0-4h4V4H1v3zm5-3v3h4V4H6zm4 4H6v3h4V8z',
      box: 'M8.186 1.113a.5.5 0 0 0-.372 0L1.846 3.5l2.404.961L10.404 2l-2.218-.887zm3.564 1.426L5.596 5 8 5.961 14.154 3.5l-2.404-.961zm3.25 1.7-6.5 2.6v7.922l6.5-2.6V4.24zM7.5 14.762V6.838L1 4.239v7.923l6.5 2.6zM7.443.184a1.5 1.5 0 0 1 1.114 0l7.129 2.852A.5.5 0 0 1 16 3.5v8.662a1 1 0 0 1-.629.928l-7.185 2.874a.5.5 0 0 1-.372 0L.63 13.09a1 1 0 0 1-.63-.928V3.5a.5.5 0 0 1 .314-.464L7.443.184z',
      folder:
        'M9.828 3h3.982a2 2 0 0 1 1.992 2.181l-.637 7A2 2 0 0 1 13.174 14H2.825a2 2 0 0 1-1.991-1.819l-.637-7a1.99 1.99 0 0 1 .342-1.31L.5 3a2 2 0 0 1 2-2h3.672a2 2 0 0 1 1.414.586l.828.828A2 2 0 0 0 9.828 3zm-8.322.12C1.72 3.042 1.95 3 2.19 3h5.396l-.707-.707A1 1 0 0 0 6.172 2H2.5a1 1 0 0 0-1 .981l.006.139z',
      'folder-check':
        'M15.854 10.146a.5.5 0 0 1 0 .708l-3 3a.5.5 0 0 1-.707 0l-1.5-1.5a.5.5 0 0 1 .707-.708l1.146 1.147 2.646-2.647a.5.5 0 0 1 .708 0z',
      'folder-x':
        'M11.854 10.146a.5.5 0 0 0-.707.708L12.293 12l-1.146 1.146a.5.5 0 0 0 .707.708L13 12.707l1.146 1.147a.5.5 0 0 0 .708-.708L13.707 12l1.147-1.146a.5.5 0 0 0-.707-.708L13 11.293l-1.146-1.147z',
      flag: 'M14.778.085A.5.5 0 0 1 15 .5V8a.5.5 0 0 1-.314.464L14.5 8l.186.464-.003.001-.006.003-.023.009a12.435 12.435 0 0 1-.397.15c-.264.095-.631.223-1.047.35-.816.252-1.879.523-2.71.523-.847 0-1.548-.28-2.158-.525l-.028-.01C7.68 8.71 7.14 8.5 6.5 8.5c-.7 0-1.638.23-2.437.477A19.626 19.626 0 0 0 3 9.342V15.5a.5.5 0 0 1-1 0V.5a.5.5 0 0 1 1 0v.282c.226-.079.496-.17.79-.26C4.606.272 5.67 0 6.5 0c.84 0 1.524.277 2.121.519l.043.018C9.286.788 9.828 1 10.5 1c.7 0 1.638-.23 2.437-.477a19.587 19.587 0 0 0 1.349-.476l.019-.007.004-.002h.001',
    };

    // Add nodes styles
    nodeEnter
      .append('path')
      .attr('d', function (d) {
        return icons[d.data.node_icon];
      })
      .attr('transform', 'translate(-8, -8)')
      .attr('class', 'node')
      .style('cursor', 'pointer')
      .style('fill', function (d) {
        return d.data.node_icon === 'folder' ? '#f8d775' : d.data.node_color;
      })
      .style('stroke', function (d) {
        return d.data.node_icon === 'folder' ? '#a9a9a9' : d.data.node_color;
      })
      .style('stroke-width', '0.5px');

    // Add check to folder
    nodeEnter
      .append('path')
      .attr('d', function (d) {
        return d.data.node_color === '#FF794A' ? icons['folder-x'] : null;
      })
      .attr('transform', 'translate(-12.5, -12)')
      .attr('class', 'node')
      .style('cursor', 'pointer')
      .style('stroke', function (d) {
        return d.data.node_color === '#FF794A' ? '#E53935' : d.data.node_color;
      })
      .style('stroke-width', '1.2px');

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

    // On exit reduce the opacity of node
    // nodeExit.select('circle').attr('r', 1e-6);
    nodeExit.select('path').style('opacity', 1e-6);

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
      .style('stroke-width', '1px')
      .style('stroke', '#ccc')
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
        // d._children = d.children;
        // d.children = null;
        collapse(d);
      } else if (d.data.node_icon === 'geo') {
        d.children = d._children;
        d._children = null;
      } else {
        expand(d);
      }
      update(d);
    }

    // ****************** legend section ***************************
    // Hardcoded legend - legend is not responsive enough
    // decide to create legend outside d3js
    // svg
    //   .append('path')
    //   .attr('d', icons['geo'])
    //   .style('fill', '#694489')
    //   .style('stroke', '#a9a9a9')
    //   .style('stroke-width', '0.5px')
    //   .attr('transform', 'translate(400, 120)');
    // svg
    //   .append('text')
    //   .attr('x', 420)
    //   .attr('y', 130)
    //   .text('Selected Project')
    //   .style('font-size', '15px')
    //   .attr('alignment-baseline', 'middle');
    // svg
    //   .append('path')
    //   .attr('d', icons['folder'])
    //   .style('fill', '#f8d775')
    //   .style('stroke', '#a9a9a9')
    //   .style('stroke-width', '0.5px')
    //   .attr('transform', 'translate(400, 150)');
    // svg
    //   .append('text')
    //   .attr('x', 420)
    //   .attr('y', 160)
    //   .text('Dataset')
    //   .style('font-size', '15px')
    //   .attr('alignment-baseline', 'middle');
    // svg
    //   .append('path')
    //   .attr('d', icons['table'])
    //   .style('fill', '#28a745')
    //   .style('stroke', '#28a745')
    //   .style('stroke-width', '0.5px')
    //   .attr('transform', 'translate(400, 180)');
    // svg
    //   .append('text')
    //   .attr('x', 420)
    //   .attr('y', 190)
    //   .text('Uploaded Data')
    //   .style('font-size', '15px')
    //   .attr('alignment-baseline', 'middle');
    // svg
    //   .append('path')
    //   .attr('d', icons['table'])
    //   .style('fill', '#E53935')
    //   .style('stroke', '#E53935')
    //   .style('stroke-width', '0.5px')
    //   .attr('transform', 'translate(400, 210)');
    // svg
    //   .append('text')
    //   .attr('x', 420)
    //   .attr('y', 220)
    //   .text('Missing Data')
    //   .style('font-size', '15px')
    //   .attr('alignment-baseline', 'middle');
  }
}
