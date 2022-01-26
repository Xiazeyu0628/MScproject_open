
let stones = []
const edges = [ 3, 4, 5, 6, 7 ]
const shades = [ 1, 2, 3, 4 ]
edges.forEach(e => {
  shades.forEach(s => stones.push(e * 10 + s))
})
const div = document.getElementById('draw');

// 比如s = 63
// isAgent true false
// scale s；default
stones.forEach(s => {
  div.append(createStone('sum', `stone-${s}`, getOpts(s, true, 's'), 'sum'))
})


