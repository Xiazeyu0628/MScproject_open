
const mode = 'exp' // '' for production, 'dev' for development, 'exp' for real exp
//change
/** Pick a condition */
//const cond = weighted_random(['A1','A2','A3','A4','A5','A6','A7','A8'], [0,0,0,0,0,0,0,0])
const cond = 'A3';
configs = configs.filter(c => c.group === cond);


// const cond = config[0].group
console.log(`${mode} mode; condition ${cond}.`);

/** Comprehension quiz */
const start_time = Date.now();
let start_task_time = 0;


// button的按键事件，第一页->第二页
document.getElementById('desc-next-btn').onclick = () => {
  hide("box-desc-1")
  showNext("box-desc-2", "block")
}

// 第二页 -> comprehension check
document.getElementById('desc-btn').onclick = () => {
  //hide("box-desc-2");
  //showNext("box-desc-1", "block");
  hide('instruction');
  showNext("comprehension", "block");
  showNext("check-btn");
}

const checkBtn = document.getElementById('check-btn');
const checks = [ 'check1', 'check2', 'check3', 'check4', 'check5' ];
const answers = [ false, false, true, true, true ];

const passBtn = document.getElementById('pass-btn');
const retryBtn = document.getElementById('retry-btn');

//map() 方法返回一个新数组，数组中的元素为原始数组元素调用函数处理后的值。
// map() 方法按照原始数组元素顺序依次处理元素
checkBtn.onclick = () => {
  let inputs = [];
  checks.map(check => {
    const vals = document.getElementsByName(check);
    inputs.push(vals[0].checked); //这里的checked意味选中的意思，0是选中的，1是未选中的。 默认的选中值是false
  });
  const pass = (inputs.join('') === answers.join(''));
  showPostCheckPage(pass);
}

passBtn.onclick = () => {
  start_task_time = Date.now();
  hide("pass");
  hide("comprehension");
  showNext("tasks", "block");
};
retryBtn.onclick = () => {
  hide("retry");
  hide("comprehension");
  showNext("instruction", "block")
};
//onchange当用户改变input输入框内容时执行一段Javascript代码
//返回true时，灰掉的checkbtn恢复了
document.getElementById('prequiz').onchange = () => compIsFilled() ? checkBtn.disabled = false : null;






/** Prep data */
let subjectData = {};

let learnSids = [];
let genSids = [];
//forEach() 方法用于调用数组的每个元素，并将元素传递给回调函数，这里的回调函数就是这个箭头函数
configs.forEach(c => (c.phase==='learn')? learnSids.push(c.sid): genSids.push(c.sid));

// Shuffle them
learnSids = shuffleArray(learnSids);
// Add two learning tasks into gen set for verification
(drawRdnNum(0, learnSids.length-1, 2)).forEach(i => genSids.push(learnSids[i]));
genSids = shuffleArray(genSids);

//传入filter一个回调函数，根据回调函数，来筛选
let learnConfigs = []
learnSids.forEach(
    (sid, idx) => {
  let cfg = (configs.filter(c => c.sid === sid))[0];
  let taskId = 'learn-' + padNum(idx+1);
  learnConfigs.push([taskId, sid, cfg.agent, cfg.recipient, cfg.result,cfg.indicator])
}
)

let genConfigs = []
genSids.forEach((sid, idx) => {
  let cfg = (configs.filter(c => c.sid === sid))[0];
  let taskId = 'gen-' + padNum(idx+1);
  genConfigs.push([taskId, sid, cfg.agent, cfg.recipient, cfg.result,cfg.indicator])
})

let trialData = {
  "sid": [],
  "agent": [],
  "recipient": [],
  "result": [],
  "predicted":[],
};
learnConfigs.forEach(c => {
  trialData['sid'].push(c[1]);  // 用来定位是哪一个规则下的哪个实验
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
  trialData['result'].push(c[4]);
})
genConfigs.forEach(c => {
  trialData['sid'].push(c[1]);
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
})

// let 声明的变量只在 let 命令所在的代码块内有效。
// const 声明一个只读的常量，一旦声明，常量的值就不能改变
let showDiv = document.getElementById("showcase");
const coreLearnDiv = document.getElementById("core-learn-div");
let learnClicked = Array(learnConfigs.length).fill(0);

for(let i = 0; i < learnConfigs.length; i++ ) {
  const taskConfig = learnConfigs[i];
  const config = {   // 这个for循环里的config是每一个configs里的值
    'taskId': taskConfig[0],   //learn-01
    'sidId' :taskConfig[1],   //A?-learn-0?
    'agent': taskConfig[2],    // 42
    'recipient': taskConfig[3],  //53
    'result': taskConfig[4] ,   //63
    'indicator':taskConfig[5]
  };

  const ruleValue =  config['sidId'].split('-')[0];


  /** Showcase contents */
  //sum 对应的是summery
  let showBox = document.getElementById(`showcase-${i+1}`); //上面6个框

  // 这个方程创建了一个容器，并且设置了容器的id（用于调用）和容器的class（用于装饰），容器内部的具体内容没有规定。
  let boxWrapper = createCustomElement("div", "sum-wrap", `${config.taskId}-sumwrap`);   //boxWrapper
  let beforeBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-before`);
  let afterBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-after`);

  beforeBox = createSumBox(beforeBox, "before", config, 'sum');
  afterBox = createSumBox(afterBox, "after", config, 'sum');

  boxWrapper.append(beforeBox);
  boxWrapper.append(afterBox);
  showBox.append(boxWrapper);
  //如果不是development开发者模式的话，boxWrapper是暂时隐藏的，只有在学习之后才会显示出来。

  boxWrapper.style.display= (mode==='dev')? 'flex': 'none';

  /** Core: learn tasks */
  const taskId = config.taskId;
  //逻辑暂未解析？？
  let display = (mode==='dev'|i===0)? 'flex': 'none';

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);

  let taskNum = createText('h2', i+1 + '/' + learnConfigs.length);
  taskBox.append(taskNum);

  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  const buttonGroup = createCustomElement("div", "button-group-vc", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-play-btn`, "Test", true));
  buttonGroup.append(createBtn(`${taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);
  box.append(taskBox);
  box.style.display = display;
  coreLearnDiv.append(box);  //

  /** Button functionalities */
  const playBtn = document.getElementById(`${taskId}-play-btn`);
  const nextBtn = document.getElementById(`${taskId}-next-btn`);

  playBtn.onclick = () => {
    playBtn.disabled = true;
    if (learnClicked[i] > 0) {
      clearStones(config);
      createInitStones(config, displayBox)
    }
    playEffects(config,ruleValue);
    setTimeout(() => {
      nextBtn.disabled = false;
      playBtn.disabled = false;
      playBtn.innerText = 'Test again'
      boxWrapper.style.display = 'flex';
    }, 2000);
    learnClicked[i] += 1;
  }
  nextBtn.onclick = () => {
    nextBtn.disabled = true;
    //如果是第6个learn example则进入learn-form部分，如果不是，则进入xia

    //let nextDiv = (i === learnConfigs.length-1)? "core-learn-form-div": `box-learn-${padNum(i+2)}`;
    let nextDiv = (i === learnConfigs.length-1)? "core-learn-form-div": `box-${learnConfigs[i+1][0]}`; //box-learn-02
    (mode !== 'dev')? hide(`box-${taskId}`): null;
    showNext(nextDiv, 'flex');
  }
}
// coreLearnDiv.style.display = 'none';

/** Core: initial input form */
let initialInput = document.getElementById("core-learn-form-div");
const initialFormName = 'initial';
initialInput.append(createTextInputPanel(initialFormName));
initialInput.style.display = (mode === '')? 'none': 'flex';

const initSubmitBtnId = `${initialFormName}-input-submit-btn`
const initSubmitBtn = document.getElementById(initSubmitBtnId);

const initInputFormId = initialFormName + '-input-form';
const initInputForm = document.getElementById(initInputFormId);


initInputForm.onchange = () => {
  let formArea = document.getElementById(initInputFormId);
  let formElements = formArea.elements;
  let textValue = formElements["initial-input"].value;
  let cetaintyValue =  formElements["initial-certainty"].value;
  // trialData.textValue = textValue;
  // trialData.cetaintyValue = cetaintyValue;

  if(textValue!="" && cetaintyValue!="--"){
    initSubmitBtn.disabled = false;
  }else{
    initSubmitBtn.disabled = true;}
}
initSubmitBtn.onclick = () => {
  let inputs = initInputForm.elements;
  //将text和ceternty输出
  Object.keys(inputs).forEach(id => subjectData[inputs[id].name] = inputs[id].value);
  initSubmitBtn.disabled = true;
  disableFormInputs(initInputFormId);
  // initInputNextBtn.disabled = false;
  hide("core-learn-form-div");
  showNext("core-gen-div")
}

initialInput.style.display = (mode==='dev')? 'flex': 'none';
// initialInput.style.display = 'none';

/** Core: generalization tasks */
let genDiv = document.getElementById("core-gen-div");
for(let i = 0; i < genConfigs.length; i++ ) {
  const taskConfig = genConfigs[i];
  const config = {
    'taskId': taskConfig[0],
    'agent': taskConfig[2],
    'recipient': taskConfig[3],
    'indicator':taskConfig[5]
  };
  const taskId = config.taskId;
  let display = (mode==='dev'|i===0)? 'flex': 'none';

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);

  let taskNum = createText('h3', `${i+1}/${genConfigs.length}:
    The changed stone will become...?`);
  let incentive = createText('h4', `$0.10 bonus for each guess you make correctly (according to the true hidden powers of the stones)`)
  taskBox.append(taskNum);

  //整个展示区域
  let displayDiv = createCustomElement("div", "display-div", `${taskId}-display-div`);
  //左边的动画区域
  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  displayDiv.append(displayBox);

  displayDiv.append(createAnswerComposer(config));

  taskBox.append(displayDiv);
  taskBox.append(incentive);

  box.append(taskBox);
  genDiv.append(box);
  box.style.display = display;

  const selectionForm = document.getElementById(`${taskId}-selection-form`);
  const confirmBtn = document.getElementById(`${taskId}-confirm-btn`);


  selectionForm.onchange = () =>
    composeSelection(`${taskId}-selection-svg`, `${taskId}-selection-form`, `${taskId}-confirm-btn`);

  confirmBtn.onclick = () => {
    let inputs = selectionForm.elements;
    let selection = {};
    Object.keys(inputs).forEach(id => selection[inputs[id].name] = inputs[id].value);
    let stoneCode = parseInt(selection.shape.slice(1,) + selection.color.slice(1,));
    trialData['predicted'].push(stoneCode);
    disableFormInputs(`${taskId}-selection-form`);
    confirmBtn.disabled = true;
    // selNextBtn.disabled = false;
    if (i < genConfigs.length-1) {
      (mode!=='dev')? hide(`box-${taskId}`): null;
      showNext(`box-gen-${padNum(i+2)}`)
    } else {
      showNext("core-gen-form-div")
    }
  }

}
genDiv.style.display = (mode==='dev')? 'flex': "none";
// genDiv.style.display = 'none';

/** Core: final input form */
let finalInput = document.getElementById("core-gen-form-div");

let fiBox = createCustomElement("div", "input-div", `final-input`);

let fiButtonGroup = createCustomElement("div", "button-group", `final-button-group`);
fiButtonGroup.append(createBtn(`final-input-submit-btn`, "OK", false));

let finalForm = createCustomElement("form", "input-form", `final-input-form`);
finalForm.innerHTML = `
  <p><b>Has your impression about how these mysterious stones work changed?</b>
    <select name="final_change" id="final_change" class="input-rule">
      <option value="--" SELECTED>
      <option value="1">Yes</option>
      <option value="0">No</option>
    </select>
  </p>`

finalInput.append(finalForm);
finalInput.append(fiButtonGroup);

const fiBtn = document.getElementById('final-input-submit-btn');

finalForm.onchange = (e) => fiBtn.disabled = (e.target.value === '--')? true: false;
fiBtn.onclick = () => {
  subjectData['final_changed'] = document.getElementById('final_change').value;
  hide("tasks")
  showNext("debrief", "block")
}


finalInput.style.display = (mode==='dev')? 'flex': "none";

/** Debrief page */
const doneBtn = document.getElementById('done-btn');
const debriefForm = document.getElementById('postquiz');

debriefForm.onchange = () => {
  isFilled('postquiz')? doneBtn.disabled = false: null;
}
doneBtn.onclick = () => {

  /**  waiting for transition*/



  /** prepare needed data */
  let inputs = debriefForm.elements;
  Object.keys(inputs).forEach(id => subjectData[inputs[id].name] = inputs[id].value);
  const end_time = new Date();
  let token = generateToken(8);
  let clientData = {};
  clientData.subject = subjectData;
  clientData.subject.condition = cond;
  clientData.subject.date = formatDates(end_time, 'date');
  clientData.subject.time = formatDates(end_time, 'time');
  clientData.subject.instructions_duration = start_task_time - start_time;
  clientData.subject.task_duration = end_time - start_task_time;
  clientData.trialData = trialData;


  const truths = genConfigs.map(c => c[4]);
  clientData.trialData.truths = truths;


  /** Give feedback */
  const predicted = trialData.predicted;
  let correct = 0;
  truths.forEach((t, i) => (t===predicted[i])? correct+=1: null);
  clientData.subject.correct = correct;

//  showCompletion(token, correct)
  hide("debrief")
  showNext("transition")
  let tran = createText('p', `Please wait for storing the experimental data.... `)
  document.getElementById('transition').appendChild(tran);
  let counter =0
  /** send to the data strorege*/
  if(mode== "exp"||mode== "dev"){
    let wso = new ChunksIncremental(
      "wss://somata.inf.ed.ac.uk/chunks/ws",
      (chunksLeft,errStatus,m) => {
        console.log("Received message: " + m);
        if(counter == 0){
          showCompletion(token, correct);
        }
        counter = 1;
        //let msgDiv = document.getElementById("messageDiv");
       // msgDiv.appendChild(document.createTextNode(m+"\n"));
        //msgDiv.appendChild(document.createElement('br'));  //<br> 可插入一个简单的换行符

      },

      (e) => {
        console.log("Encountered error: " + e);
        let errorDiv = document.getElementById("messageDiv");
        errorDiv.appendChild(document.createTextNode(e+"\n"));
        errorDiv.appendChild(document.createElement('br'));
      }

  );

    let sessionId = token;

// Experiment ids should be descriptive strings. For UG4/MInf/MSc projects,
// they should include the surname of the experimenter.
    let experimentId = "mysteriousStone_exp";

// Send a minimalist message, with just a timestamp.  发送一个极简的消息，只有一个时间戳。
// In a real experiment, this would be replaced with everything that is being recorded.

    let message = {sessionId: sessionId, experimentId: experimentId, experimentResult: clientData};
    wso.sendChunk(message);

    //setTimeout(wso.closeSocket(),10000)
  }

};
