// Instances of the class "ChunksIncremental" manage
// sending data and re-creating any closed websocket connections.
//ChunksIncremental”的实例管理发送数据和重新创建任何关闭的websocket连接。
class ChunksIncremental {   //块增量
    constructor(url, msg_callback, err_callback) {

        this.wso = null;

        this.url = url;

        this.sentChunks = 0;

        this.completedChunks = 0;

        this.messageCallback = msg_callback;

        this.errorCallback = err_callback;

        this.wsError = false;

        this.msgs = [];

        this.buildWs();  //buildWebsocket

    }

    buildWs() {

        this.wso = new WebSocket(this.url);   //websocket object

// We want to send any messages that accumulated while the  connection might have been down.
//我们希望发送在连接可能已经关闭时累积的任何消息。
        this.wso.onopen = () => this.sendAll();  // 连接建立时触发

        this.wso.onmessage = event => {  // onmessage 事件来接收服务器返回的数据，客户端接收到服务端数据时触发。
            // If we are just printing, don't deserialize via JSON.parse //如果只是打印，不要通过JSON.parse进行反序列化
           const message = event.data;
           if (message.status == "SUCCESS") {
                    this.completedChunks += 1;}
           this.messageCallback(this.sentChunks - this.completedChunks, this.wsError, message);
           };

        this.wso.onerror = error => { //通信发生错误时触发
            console.error(`Websocket error detected: ${JSON.stringify(error)}`);
            this.wsError = true;
            this.errorCallback(error);

        };

        this.wso.onclose = () => {  //连接关闭时触发
            console.error("Ws closed; re-creating");
            this.wso = null;
            setTimeout(this.buildWs(), 4000);
        };

    }


    sendChunk(dataChunk) {

        const {experimentId, sessionId} = dataChunk;

        if (experimentId === null || sessionId == null) {

            console.log("Reguires session and experiment id.");

        }
        //JSON.stringify() 方法用于将 JavaScript 值转换为 JSON 字符串。
        const dataStr = JSON.stringify(dataChunk);

        if (this.wso.readyState !== 1) {   //1 - 表示连接已建立，可以进行通信

            this.msgs = [...this.msgs, dataStr];   // 连接未建立，先把要发送的东西储存在msgs数组中

        } else {

            this.wso.send(dataStr);

            this.sentChunks += 1;

        }

    }


    sendAll() { // less validation than sendChunk. Used to clear message backlog.
        //验证少于sendChunk。用于清除邮件积压。

        while (this.wso.readyState == 1 && this.msgs.length > 0) {

            this.wso.send(this.msgs.pop());

            this.sentChunks += 1; }

        if (this.msgs.length > 0) {

            console.error("Did not send all messages, remaining: " + this.msgs.length)

        }

    }


    closeSocket(){
        this.wso.close()
    }

}