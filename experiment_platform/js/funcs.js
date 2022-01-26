const svgElements = [ "svg", "polygon", "circle", "rect", "path" ];
const defaultStone = { 'borderWidth': '8px', 'mar': 5, 'len': 60 };
const smallStone = { 'borderWidth': '3px', 'mar': 3, 'len': 20 };

const shuffleSelecter = Math.random() > 0.5

/** Configurations */
const colorDict = {
  "light": "#c9daf8",
  "medium": "#6d9eeb",
  "dark": "#1155cc",
  "very_dark": "#052e54",
}
const allColors = Object.keys(colorDict);

// 比如s = 63
// isAgent true false
// scale s；default



/** 由数据获取options，options将会被用于create stone */
function getOpts(int, isAgent, scale='default') {
  const edges = Math.floor(int / 10);
  const shading = int % 10;

  let opts = {};
  opts["color"] = colorDict[Object.keys(colorDict)[shading-1]]
  opts["hasBorder"] = isAgent;
  opts["points"] = calcPolygon(edges, scale);
  opts["scale"] = scale;
  return opts
}

function calcPolygon(n, scale) {
  n = parseInt(n);
  let output = [];
  let adjust = (n===5)? 55 : 0;

  const mar = (scale==='default')? defaultStone.mar: smallStone.mar;
  const len = (scale==='default')? defaultStone.len: smallStone.len;

  if (n === 3) {
    output.push(`${len/2},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else if (n === 4) {
    output.push(`${mar},${mar}`);
    output.push(`${len-mar},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else {
    // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
    for (let i = 1; i <= n; i++) {
      output.push(
          ((len/2 * Math.cos(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString() + "," +
          ((len/2 * Math.sin(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString()
      )
    }
  }
  return output.join(" ")
}


/** fundamental building stone method */

// 这个方程创建了一个容器，并且设置了容器的id（用于调用）和容器的class（用于装饰）
//document.createElement创建出来的节点是属于html dom，
// 而svg的节点是svg dom，所以需要用createElementNS函数并传入节点名称的命名空间。
function createCustomElement (type = 'div', className, id) {
  let element = (svgElements.indexOf(type) < 0)?  //如果不是svg元素，直接用createElement
      document.createElement(type):
      document.createElementNS("http://www.w3.org/2000/svg", type);
  if (className.length > 0) element.setAttribute("class", className);
  element.setAttribute("id", id);
  return element;
}


//attach stone 函数用于在svg容器/节点里添加多边形的
function attachStone (svg, id, opts, shapeClass = 'shape') {
  if (Object.keys(opts).indexOf("points") < 0) {
    if (Object.keys(opts).indexOf("d") < 0) {
      svg.append(createCircle(shapeClass, `${id}`, opts));
    } else {
      svg.append(createDonut(shapeClass, `${id}`, opts));
    }
  } else {
    svg.append(createPolygon(shapeClass, `${id}`, opts))
  }
  return svg
}

function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  let borderWidth = (opts.scale==='default')? defaultStone.borderWidth: smallStone.borderWidth;
  setAttributes(polygon, {
    "fill": opts.color,   //颜色
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  });
  return(polygon);
}

//用于创建svg和div节点，在svg中添加多边形，并放入div节点中
function createStone (stoneClass, id, opts, svgClass = 'test') {
  let div = createCustomElement("div", stoneClass, id);
  let svg = createCustomElement("svg", svgClass, `${id}-svg`);  //创建一个svg节点
  svg = attachStone(svg, `${id}-stone`, opts); // -stone是svg节点里的多边形
  div.append(svg)
  return(div);
}

/** showcase*/

function createSumBox(sumBox, type, config, stoneClass) {  //createSumBox(beforeBox, "before", config, 'sum');
  let ruleValue =  config['sidId'].split('-')[0];
  let textDiv = createText("h2", capFirstLetter(type));
  let sumDiv = createCustomElement("div", "sum-display", `${config.taskId}-sumbox-${type}-display`);
  if (type === "before") {
    sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-before-agent`, getOpts(config.agent, config.indicator, 's'), 'sum'));
    sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-before-recipient`, getOpts(config.recipient, false, 's'), 'sum'));
    sumDiv.style.justifyContent = "space-between";
  } else if (type === "after") {
      if(ruleValue=='A1'||ruleValue=='A5'){
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-agent`, getOpts(config.result, config.indicator, 's'), 'sum'));
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-result`, getOpts(config.recipient, false, 's'), 'sum'));
        sumDiv.style.justifyContent = "flex-end";
      }else if(ruleValue=='A2'||ruleValue=='A6'){
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-agent`, getOpts(config.agent, config.indicator, 's'), 'sum'));
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-result`, getOpts(config.result, false, 's'), 'sum'));
        sumDiv.style.justifyContent = "flex-start";
      }else if(ruleValue=='A3'||ruleValue=='A7'){
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-agent`, getOpts(config.agent, config.indicator, 's'), 'sum'));
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-result`, getOpts(config.result, false, 's'), 'sum'));
        sumDiv.style.justifyContent = "flex-end";
      }else if(ruleValue=='A4'||ruleValue=='A8'){
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-agent`, getOpts(config.agent, config.indicator, 's'), 'sum'));
        sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-result`, getOpts(config.result, false, 's'), 'sum'));
        sumDiv.style.justifyContent = "flex-end";
      }





  } else {
    console.log("Summary type not match @createSummaryStones()")
  }
  sumBox.append(textDiv);
  sumBox.append(sumDiv);
  return sumBox;
}

/** core learn items create*/

function createText(h = "h1", text = 'hello') {
  //创建指定文本的h你需要在按钮元素后添加文本节点
  let element = document.createElement(h);
  let tx = document.createTextNode(text);
  element.append(tx);
  return(element)
}

function createInitStones(config, parentDiv) {
  parentDiv.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, config.indicator)));
  parentDiv.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
  return(parentDiv);
}

//用于learning/gen中的动画区
// function createStones (config) {
//   let el = document.getElementById(`${config.taskId}-display-box`);
//   el.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
//   el.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
//   return(el)
// }

function createBtn (btnId, text = "Button", on = true, className = "task-button") {
  let btn = createCustomElement("button", className, btnId);
//   disabled 属性可设置或返回是否禁用按钮。
//
// 禁用的元素是不可用的，也无法点击。禁用元素在浏览器中通常显示灰色。
  btn.disabled = !on;
  (text.length > 0) ? btn.append(document.createTextNode(text)): null;
  return(btn)
}

/** core task items interact and animate */

function playEffects (config,ruleValue) {
  const getCurrentLocation = (id) => {
    let rect = {top: 0, bottom: 0, left: 0, right: 0};
    // rect= rectangular
    //getBoundingClientRect用于获取某个元素相对于视窗的位置集合。集合中有top, right, bottom, left等属性
    const pos = document.getElementById(id).getBoundingClientRect();
    rect.top = pos.top;
    rect.bottom = pos.bottom;
    rect.left = pos.left;
    rect.right = pos.right;
    return rect;
  }
  // 从document这个DOM对象中拿body这个HTML元素对象，检测这个元素对象下是否包含id为learn-01-agent的对象元素
  // if (!(document.body.contains(document.getElementById(`${config.taskId}-agent`)))) {
  //   createStones(config) //如果没有找到，就去重新创建
  // }
  const agent = `${config.taskId}-agent`;
  const recipient = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const recipientStone = document.getElementById(recipient);
  const agentPos = getCurrentLocation(agent);
  const recipientPos = getCurrentLocation(recipient);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipient).left;

  // if(ruleValue=='A1'||ruleValue=='A4'){
  //   const delta = Math.round(endPos - startPos)/2;  //这里决定了移动的范围
  //   agentStone.style.left = `${delta}px`; //这里决定了移动
  //   recipientStone.style.left = `${-delta}px`;
  //
  //   setTimeout(() => {
  //     let svg = document.getElementById(`${config.taskId}-recipient-svg`);
  //     clearElement(`${config.taskId}-recipient-stone`);
  //     svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
  //   }, 1500);
  //
  // }else if(ruleValue=='A2'||ruleValue=='A5'){
  //   const delta = Math.round(endPos - startPos)+15;  //这里决定了移动的范围
  //   agentStone.style.left = `${delta}px`; //这里决定了移动
  //
  //   setTimeout(() => {
  //     clearElement(`${config.taskId}-recipient-stone`);  //清除的是div节点里的svg节点
  //     clearElement(`${config.taskId}-agent-stone`);
  //     let parentDiv= document.getElementById(`${config.taskId}-display-box`)
  //     let resultDiv = createStone("new-stone", `${config.taskId}-result`, getOpts(config.result, false));
  //     parentDiv.insertBefore(resultDiv,recipientStone)
  //     resultDiv.style.left = "110px";
  //   }, 1500);
  //
  // }else if(ruleValue=='A3'||ruleValue=='A6'){
  //
  //   const delta = Math.round(endPos - startPos)/2;  //这里决定了移动的范围
  //   agentStone.style.left = `${delta}px`; //这里决定了移动
  //   recipientStone.style.left = `${-delta}px`;
  //
  //   setTimeout(() => {
  //     clearElement(`${config.taskId}-recipient-stone`);  //清除的是div节点里的svg节点
  //     clearElement(`${config.taskId}-agent-stone`);
  //     let parentDiv= document.getElementById(`${config.taskId}-display-box`)
  //     let resultDiv = createStone("new-stone", `${config.taskId}-result`, getOpts(config.result, false));
  //     parentDiv.insertBefore(resultDiv,recipientStone)
  //
  //   }, 1500);
  // }
  if (ruleValue == 'A1' || ruleValue == 'A5') {  //statechange：agent从左运动到右，结果出现在agent的位置上
    const delta = Math.round(endPos - startPos) + 15;  //这里决定了移动的范围
    agentStone.style.left = `${delta}px`; //这里决定了移动

    setTimeout(() => {
      let svg = document.getElementById(`${config.taskId}-agent-svg`);
      clearElement(`${config.taskId}-agent-stone`);
      svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, true));
    }, 1500);
  } else if (ruleValue == 'A2' || ruleValue == 'A6') {  // movement: recipient 从右向左，结果出现recipient的位置上
    const delta = Math.round(endPos - startPos) + 15;  //这里决定了移动的范围
    recipientStone.style.left = `${-delta}px`; //这里决定了移动

    setTimeout(() => {
      let svg = document.getElementById(`${config.taskId}-recipient-svg`);
      clearElement(`${config.taskId}-recipient-stone`);
      svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
    }, 1500);

  } else if (ruleValue == 'A3' || ruleValue == 'A7') {// 没有明确的指示谁是agent/recipient=

    const delta = Math.round(endPos - startPos) + 15;  //这里决定了移动的范围
    agentStone.style.left = `${delta}px`; //这里决定了移动

    setTimeout(() => {
      let svg = document.getElementById(`${config.taskId}-recipient-svg`);
      clearElement(`${config.taskId}-recipient-stone`);
      svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
    }, 1500);
  }
  else if (ruleValue == 'A4' || ruleValue == 'A8') {// original agent 从左向右，recipient变化

      const delta = Math.round(endPos - startPos)+15;  //这里决定了移动的范围

      agentStone.style.left = `${delta}px`; //这里决定了移动

      setTimeout(() => {
        let svg = document.getElementById(`${config.taskId}-recipient-svg`);
        clearElement(`${config.taskId}-recipient-stone`);
        svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
      }, 1500);
  }

}
//这个函数用于点击重新播放时，清除上一次动画的残留
  function clearStones(config) { //els = elements
    const ruleValue = config['sidId'].split('-')[0];
    if (ruleValue == 'A4' || ruleValue == 'A8') {
      let els = ["agent", "recipient", "result"].map(s => `${config.taskId}-${s}`);

      els.forEach(el => clearElement(el));
    } else {
      let els = ["agent", "recipient"].map(s => `${config.taskId}-${s}`);
      els.forEach(el => clearElement(el));
    }
  }

  function clearElement(id) {
    let clear = document.getElementById(id);
    clear.remove();
  }


  /** core learn form div*/

  function createTextInputPanel(taskId) {
    let taskBox = createCustomElement("div", "input-div", `${taskId}-input`);

    const displayBox = createCustomElement("div", "input-box", `${taskId}-input-box`);
    displayBox.append(createInputForm(taskId));

    const buttonGroup = createCustomElement("div", "button-group-vc", `${taskId}-button-group`);
    buttonGroup.append(createBtn(`${taskId}-input-submit-btn`, "OK", false));

    taskBox.append(displayBox);
    taskBox.append(buttonGroup);
    return taskBox;
  }

  function createInputForm(taskId) {
    let form = createCustomElement("form", "input-form", `${taskId}-input-form`);
    const placeholderText = `Please type here`
    const options = `
    <option value="--" SELECTED>--</option>
    <option value="10">10 - Very certain</option>
    <option value="9">9</option>
    <option value="8">8</option>
    <option value="7">7</option>
    <option value="6">6</option>
    <option value="5">5 - Moderately</option>
    <option value="4">4</option>
    <option value="3">3</option>
    <option value="2">2</option>
    <option value="1">1</option>
    <option value="0">0 - Not sure at all</option>
  `
    form.innerHTML = `
    <p>
      <b>What is your best guess about how these mysterious stones work?</b>
      (Please refer to stones as <i>active</i> and <i>inactive</i>,
      and be specific about <i>what properties you think matter or do not matter for the effects,
      and how they do so</i>.)
      <br />
    </p>
    <textarea name="${taskId}_input" id="${taskId}-input" placeholder="${placeholderText}"></textarea>
    <p class="incentive">Remember there is a $0.50 bonus if you guess correctly, and nonsense answers will result in a zero bonus or hit rejection.</p>
    <p>How certain are you?
      <select name="${taskId}_certainty" id="${taskId}-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    `
    return (form);
  }

  /** generalization */

  function createAnswerComposer(config) {
    const taskId = config.taskId;
    let box = createCustomElement("div", "display-box", `${taskId}-selection-box`);
    box.style.width = "48%";

    // p标签paragraph，b标签bold
    const sidesSelector = `<p><b>Sides</b>:    
    <select id="shape" name="shape" class="selection-input">
      <option value="--" SELECTED>--</option>
      <option value="p3">3 (triangle)</option>
      <option value="p4">4 (square)</option>
      <option value="p5">5 (pentagon)</option>
      <option value="p6">6 (hexagon)</option>
      <option value="p7">7 (heptagon)</option>
    </select></p>`;
    const shadesSelector = `<p><b>Shading</b>:
    <select id="color" name="color" class="selection-input">
      <option value="--" SELECTED>--</option>
      <option value="s1" style="background-color:red">Light</option>
      <option value="s2">Medium</option>
      <option value="s3">Dark</option>
      <option value="s4">Very dark</option>
    </select></p>`
    box.innerHTML = `
    <div class="selection-composer">
      <div class="selection-svg-div">
        <svg class="selection-object" id='${taskId}-selection-svg'></svg>
      </div>
      <div class="selection-form-div">
        <form class="selection-form" id="${taskId}-selection-form">
        ${shuffleSelecter ? sidesSelector : shadesSelector}
        ${shuffleSelecter ? shadesSelector : sidesSelector}
        </form>
      </div>
      <div class="selection-buttons">
        <button class="task-button" id="${taskId}-confirm-btn" disabled>OK</button>
      </div>
    </div>`
    return box;
  }

// Object.keys 返回一个所有元素为字符串的数组，其元素来自于从给定的object上面可直接枚举的属性
// 这些属性的顺序与手动遍历该对象属性时的一致
  function currentSelection(formId) {
    let selections = [];
    const inputs = document.getElementById(formId).elements;
    (Object.keys(inputs)).forEach((input) => {
      selections.push(inputs[input].value)
    });
    return selections.reverse().join(";")
  }

  function composeSelection(svgid, formid, checkBtnId) {
    const selections = currentSelection(formid).split(";");
    const color = shuffleSelecter ? selections[0] : selections[1];
    const shape = shuffleSelecter ? selections[1] : selections[0];
    const taskId = svgid.split('-').slice(0, 2).join("-");

    if (!(color === "--" || shape === "--")) {
      let stoneCode = parseInt(shape.slice(1,) + color.slice(1,))  //43
      let svg = document.getElementById(svgid);
      if (svg.childNodes.length > 0) {
        clearElement(`${taskId}-test-stone`)
      }
      ;
      svg = attachStone(svg, `${taskId}-test-stone`, getOpts(stoneCode));

      let checkBtn = document.getElementById(checkBtnId);
      checkBtn.disabled = false;
    }
    if ((color === '--' || shape === '--')) {
      clearElement(`${taskId}-test-stone`)
      let checkBtn = document.getElementById(checkBtnId);
      checkBtn.disabled = true;
    }

  }


  function createDivWithStyle(className = "div", id = "", style = "") {
    let element = createCustomElement('div', className, id);
    setStyle(element, style);
    return element;
  }

  function setAttributes(el, attrs) {
    for (var key in attrs) {
      el.setAttribute(key, attrs[key]);
    }
  }

  function disableFormInputs(formId) {
    const form = document.getElementById(formId);
    const inputs = form.elements;
    (Object.keys(inputs)).forEach((input) => inputs[input].disabled = true);
  }

  function showNext(id, display = "flex") {
    let div = document.getElementById(id);
    div.style.display = display;
    div.scrollIntoView(true);
  }

  function hide(id) {
    let div = document.getElementById(id);
    div.style.display = "none";
  }

  function isFilled(formID) {
    let notFilled = false;
    const nulls = ['', '--', '', '--', '', '--'];
    const form = document.getElementById(formID);
    const inputs = form.elements;
    (Object.keys(inputs)).forEach((input, idx) => {
      let field = inputs[input];
      notFilled = (notFilled || (field.value === nulls[idx]));
    });
    return (!notFilled)


  }


  function capFirstLetter(str) {
    let fl = str[0].toUpperCase();
    return (fl + str.slice(1,));
  }


  function shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1)); //Math.floor() 返回小于或等于一个给定数字的最大整数
      //Math.random() 函数返回一个浮点数,  伪随机数在范围从0到小于1，
      [array[i], array[j]] = [array[j], array[i]];
    }
    return array
  }

  function drawRdnNum(lower = 1, upper = 6, n = 2) {
    const drawOne = (lower, upper) => Math.floor(Math.random() * (upper - lower + 1)) + lower;
    let nums = [];
    nums.push(drawOne(lower, upper))
    if (n > 1) {
      while (nums.length < n) {
        let m = drawOne(lower, upper);
        (nums.indexOf(m) < 0) ? nums.push(m) : null; // indexof用于返回值为m的第一个index的值，如果没有这个值，则返回-1
        //这里是为了防止取到两个一样的m
        //The indexOf() method returns the first index at which a given element can be found in the array,
        // or -1 if it is not present
      }
    }
    return (nums)
  }

  function padNum(counter, n = 2) {
    return (counter.toString().padStart(n, '0'))
  }

  function showPostCheckPage(isPass) {
    const pageDiv = isPass ? 'pass' : 'retry';
    document.getElementById('check-btn').style.display = 'none';
    document.getElementById(pageDiv).style.display = 'grid';
  }

  function compIsFilled() {
    let radios = document.getElementsByTagName('input');
    let checked = 0;
    for (let i = 0; i < radios.length; i++) {
      checked += radios[i].checked;
    }
    //radio 一共有10个选圈，只有选了5个的时候才能返回true
    return (checked > checks.length - 1)
  }

  function showCompletion(code, nCorrect) {
    hide("transition")
    showNext("completed")
    let t = document.createTextNode(code);
    let co = createText('p', `You got ${nCorrect} predictions correct!
  You will get $${nCorrect * 0.1} bonus on top of your base pay.
  Bonus on writing the correct causal power will be paid after manual checks.`)
    document.getElementById('completion-code').append(t);
    document.getElementById('completed').append(co);
  }

  function generateToken(length) {
    let tokens = '';
    let chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    for (let i = 0; i < length; i++) {
      tokens += chars.charAt(Math.floor(Math.random() * chars.length))
    }
    return tokens;
  }

  function formatDates(date, option = 'date') {
    let year = date.getFullYear();
    let month = String(date.getMonth() + 1).padStart(2, '0');
    let day = String(date.getDate() + 1).padStart(2, '0');
    let hour = String(date.getHours() + 1).padStart(2, '0');
    let min = String(date.getMinutes() + 1).padStart(2, '0');
    let sec = String(date.getSeconds() + 1).padStart(2, '0');
    dateParts = (option === 'date') ? [year, month, day] : [hour, min, sec];
    return dateParts.join('_');
  }

  function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], {type: contentType});
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
  }

  function weightedChoice(array, weights) {
    let s = weights.reduce((a, e) => a + e);
    //reduce() 方法接收一个函数作为累加器，数组中的每个值（从左到右）开始缩减，最终计算为一个值。
    //array.reduce(function(total, currentValue, currentIndex, arr), initialValue)
    //其中total为初始值或者计算后的返回值（必须）、currentValue为当前元素（必须）

    // find() 方法返回通过测试（函数内判断）的数组的第一个元素的值。
// find() 方法为数组中的每个元素都调用一次函数执行：
// 当数组中的元素在测试条件时返回 true 时, find() 返回符合条件的元素，之后的值不会再调用执行函数。
// 如果没有符合条件的元素返回 undefined
    let r = Math.random() * s; // 返回介于 0（包含） ~ 1（不包含） 之间的一个随机数：
    return array.find((e, i) => (r -= weights[i]) < 0);   //element ，index
  }


  function weighted_random(items, weights) {

    for (let i = 0; i < weights.length; i++)
      weights[i] += weights[i - 1] || 0;

    let randomValue = Math.random() * weights[weights.length - 1];

    for (let i = 0; i < weights.length; i++)
      if (weights[i] > randomValue) {
        return items[i];
      }

  }
