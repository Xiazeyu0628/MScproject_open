
/** This script generates simuli configuration for Experiment A1-A6:
 * Conditions: 2 x 3 design:
 *  A1~A6 have same ground truth edges(A)+1, shades(R)+1;
 *  A1 & A2 & A3 & A4 fix constant Agent, vary Recipient systematically;
 *  A5 & A6 & A7 & A8 fix constant Recipient, vary Agent systematically;
 *  A1 & A5  stateChange
 *  A2 & A6  movement
 *  A3 & A7  indicator
 *  A4 & A8  original
 * Object representation:
 *  Two digits integer:
 *    tens digit for number of edges (3 - triangle, 4 - square, 5, 6, 7);
 *    units digit for shading degree (1 - very light, 2 - medium, 3 - dark, 4 - v. dark).
 *  Eg. 31 for very_light triangle
 * Usage:
 *  Run this script _four_ times to setup four experiment configurations.
 *  Name each configuration w.r.t experiment condition.
 *  Do not change the generated config object name - see how task.js consumes it.
 */

let config = [];
const condition = 'all';

//learning
const fixed_obj = 42;
const varied_obj = [ 31, 53, 62, 33, 52, 61 ].sort();

//generalization
const contrast_fixed_obj = [ 32, 43, 61, 42 ].sort();
const contrast_varied_obj = [ 32, 41, 51, 63 ].sort();
const condition_number = 8
/** Generate experiment configs */
if (condition=='all') {
  // - see setup.html   这里要改i的数
  for (let i = 1; i <= condition_number; i++) {
    let cond = 'A' + i;
    config = config.concat(getExpConfig(cond))
  }
} else {
  // Save it to a `config_[condition].js` file
  config = getExpConfig(condition)
}

print(config)

function formatConfig (idx, group, phase, agent, recipient, rule,indicator) {
  const padNum = (num) => num.toString().padStart(2, '0');
  let config = {};
  config['sid'] = `${group}-${phase}-${padNum(idx)}`
  config['group'] = group;
  config['phase'] = phase;
  config['trial'] = idx;
  config['agent'] = agent;
  config['recipient'] = recipient;
  config['indicator'] = indicator;
  config['result'] = fetchResult(agent, recipient, rule);
  return config
}

/** Functions that do the real job */

function getExpConfig (condition) {
  let configArr = [];

  // Learning trials
  const rule = "AR"
 // const indicator = (condition[1] <= condition_number/4 || (condition[1] <= 3*condition_number/4)&&(condition[1] > condition_number/2) )? 1:0;
  const indicator = (condition[1] == 3|| condition[1] ==7 )? 0:1;
  if (condition[1] <= condition_number/2 ) {
    // Fix Agent, vary Recipient  A1-4
    varied_obj.forEach((r, idx) => {
      configArr.push(formatConfig(idx+1, condition, 'learn', fixed_obj, r, rule,indicator))
    })
  } else {  // Fix Recipient, vary Agent
    varied_obj.forEach((r, idx) => {
      configArr.push(formatConfig(idx+1, condition, 'learn', r, fixed_obj, rule,indicator))
    })
  }

  // Generalization trials
  let pairs = [];
  const gen_agents = (condition[1] <= condition_number/2 )? contrast_fixed_obj: contrast_varied_obj;
  const gen_recipients = (condition[1] <= condition_number/2 )? contrast_varied_obj: contrast_fixed_obj;

  gen_agents.forEach(a => {
    gen_recipients.forEach(r => pairs.push([a, r]))
  })
  pairs.forEach((pair, idx) => {
    configArr.push(formatConfig(idx+1, condition, 'gen', pair[0], pair[1], rule,indicator))
  })

  return configArr

}

function fetchResult (agent, recipient, rule) {
  const readShades = (obj) => obj % 10
  const readEdges = (obj) => Math.floor(obj / 10)
  switch (rule) {
    case "AR":
      return (readEdges(agent)+1) * 10 + (readShades(recipient)+1);
    case "RA":
      return (readEdges(recipient)+1) * 10 + (readShades(agent)+1);
    default:
      return 0
  }
}
