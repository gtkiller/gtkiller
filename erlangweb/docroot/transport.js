// namespace
cu = window.cu || {};

cu.transport = cu.transport || {};

// error codes definitions
var ERR = {
    NO_ERROR:           0,  // No errors
    INVALID_RESPONSE:   51, // Response is obtained, but format is incorrect
    CONNECTION_LOST:    52, // User network adapter is not connected
    TIMEOUT_ABORT:      53, // Server is not responding and timer on a client side aborts request
    HTTP_ERROR:         54  // When HTTP response status is not 200
};

/**
 * Class helps to send/retreive data via Unison JSON protocol
 *
 * @author Dmitry Korlas
 */

/**
 * @hashmap TJSONRequestParams Defines a request parameters
 *
 * @param {String} url Address to which a request will be sent
 * @param {Array/HashMap} commands A list or a single command to sent; optional
 * @param {FJSONRequestCalback} success Function, that will to be invoked on valid response; optional
 *      Valid response is response, in which exists least one command,
 *      and response body has "success" field value equals to true
 *      and response body has no "error" field, or "error" field equals 0
 * @param {FJSONRequestCalback} failure Function, that will to be invoked on Transport error,
 *              or in case if response body contains a not zero "error" field; optional
 * @param {Object} context Context in which callbacks will be invoked (default to window); optianl
 */

/**
 * @function FJSONRequestCalback Defines an callback signature of JSONRequest
 *
 * @param {JSONRequest} oRequest Request object
 */

/**
 * @constructor Creates new instance
 *
 * @param {TJSONRequestParams} oParams Request parameters
 * @public
 */
cu.transport.JSONRequest = function(oParams)
{
    /**
     * @property {Array} commands that must to be be sent
     * @private
     */
    this.commands = [];

    /**
     * @property {TJSONRequestParams} params Parameters that was passed in constructor
     * @private
     */
    this.params = oParams;

    /**
     * @property {HashMap} response Store raw response
     * @private
     */
    this.response = null;

    /**
     * @property {HashMap} result stores Parset response
     * @private
     */
    this.result = null;

    /**
     * @property {Number} error Error code
     * @public
     */
    this.error = ERR.NO_ERROR;

    if (oParams.commands)
        this.commands = oParams.commands;
};

cu.transport.JSONRequest.prototype = {

    /**
     * request method
     */
    method: 'POST',

    /**
     * Period, after that the client considers, that server is not responding
     */
    timeout: 30000, // 30 sec

    /**
     * Attach new comand to request
     *
     * @param {HashMap} oCommand Command body
     * @public
     */
    addCommand: function(oCommand)
    {
        this.commands.push(oCommand);
    },

    /**
     * Send a request
     *
     * @public
     */
    send: function()
    {
        jQuery.ajax({
            url: this.params.url,
            type: this.method,
            data: 'request=' + this.collectJsonRequestBody(),
            success: this.processSuccessfullResponse, // internal callback
            error: this.processFailedResponse, // internal callback
            //complete: this.processRequestComplete,
            timeout: this.timeout,
            context: this
        });
    },

    /**
     * Collects a json for request
     *
     * @return {HashMap} oRequestBody Json structure are to be posted to the server-side
     * @private
     */
    collectJsonRequestBody: function()
    {
        return $.toJSON({
            commands: this.commands
        });
    },

    /*
    processRequestComplete: function(oXMLHttpRequest, sTextStatus)
    {
        console.log(sTextStatus);
        var b = 3;
    },
    */

    /**
     * Internal callback, that called when response was retrieved
     * @todo define in which cases it's called
     *      -data is valid and parsed into js obj?
     *
     * @see jQuery.ajax() "success" definition
     * @param {HashMap} oData Response from the server-side
     * @param {String} sTextStatus Response status
     * @param {XMLHttpRequest} oXMLHttpRequest Transport object
     * @private
     */
    processSuccessfullResponse: function(oData, sTextStatus, oXMLHttpRequest)
    {
        // store raw response
        this.response = oXMLHttpRequest;

        //var oResult = cu.ResponseParser.getResponse(oXMLHttpRequest);
        var
            oParams     = this.params,
            oHandler    = oParams.success;

        // user parser to converts response string to the result object
        this.result = this.processResponse(oXMLHttpRequest);

        if (this.result.error && this.result.error != ERR.NO_ERROR)
        {
            this.error          = this.result.error;
            this.description    = this.result.description;

            oHandler    = oParams.failure;
        }

        if (typeof oHandler == 'function')
        {
            // call user passed callback if defined
            oHandler.apply(oParams.scope || window, [this]);
        }
    },

    /**
     * Internal callback, that called when request fails
     * @todo define in which cases it's called
     * @todo review error detecting, is not like in ExtJS
     *
     * @see jQuery.ajax() "error" definition
     * @param {XMLHttpRequest} oXMLHttpRequest Transport object
     * @param {String} sTextStatus Type of error that was occurred
     * @param {String} oErrorThrown Exception object
     * @private
     */
    processFailedResponse: function(oXMLHttpRequest, sTextStatus, oErrorThrown)
    {
        var oParams = this.params;

        // store raw response
        this.response = oXMLHttpRequest;

        switch (sTextStatus)
        {
            case "timeout":
                this.error = ERR.TIMEOUT_ABORT;
                break;
            case "error":
                break;
            case "notmodified":

                break;
            case "parsererror":
                break;
            default:
                break;
        } // switch

        if (!this.error == ERR.TIMEOUT_ABORT)
        {
            switch (oXMLHttpRequest.status)
            {
                case 0: // is connection lost exception
                case 12007: // ERROR_INTERNET_NAME_NOT_RESOLVED
                case 12029: // ERROR_INTERNET_CANNOT_CONNECT
                    this.error = ERR.CONNECTION_LOST;
                    break;

                case -1: // aborting by timeout occured
                case 12002: // ERROR_INTERNET_TIMEOUT
                    this.error = ERR.TIMEOUT_ABORT;
                    break;

                default:
                    this.error = ERR.HTTP_ERROR; // status is not 200
            }
        }

        // call user passed callback if defined
        if (typeof oParams.failure == 'function')
            oParams.failure.apply(oParams.scope || window, [this]);
    },

    /**
     * Converts JSON text in JavaScript object
     *
     * @param {XMLHttpRequest} oTransport Transport object containing response text
     * @return {TBaseResponse} Response object
     * @protected
     */
    processResponse: function(oTransport)
    {
        var oResponse = cu.transport.ResponseParser.getResponse(oTransport);
        //oResponse.key = this.key;

        // store raw response
        this.response = oTransport;

        return oResponse;
    },

    /**
     * Extract response by specified index
     *
     * @param {Integer} nIndex Position of response
     * @return {TAbstractCommandResponse/null} oCommandResponse A response of executed command or null
     * @public
     */
    getResponseAt: function(nIndex)
    {
        return (nIndex >= 0 && nIndex < this.result.commands.length)
            ? this.result.commands[nIndex]
            : null;
    }
};



/**
 * @singleton Parser of response in JSON format
 *
 * @author Alexander Belousov
 */
cu.transport.ResponseParser = {

    /**
     * Gets the response as JavaScript object from
     * the XMLHttpRequest response text
     *
     * @param {XMLHttpRequest} oTransport response transport
     * @return {TResponse/TErrorResponse} parsed response
     * @public
     */
    getResponse: function(oTransport)
    {
        return this.parseJsonString(oTransport.responseText);
    },

    /**
     * Parse the response from passed json string
     *
     * @param {String} sJson JSON string to parse
     * @return {TResponse/TErrorResponse} object as result of parsed string
     * @see protocol response declaration
     */
    parseJsonString: function(sJson)
    {
        try
        {
            var
                aCommands, nLength, nIndex,
                oResponse   = eval('(' + sJson + ')');

            //if (!('success' in oResponse))
            //  throw new Error();

            if (!oResponse.commands)
                throw new Error();

            // no other checking needs

            /*
            if (oResponse.success)
            {
                //aCommands = oResponse.commands;
                aCommands = oResponse.results;

                //@todo implements checking that "aCommands" is an array
                if (!aCommands || aCommands.length == 0)
                    throw new Error();

                for (nIndex = 0, nLength = aCommands.length; nIndex < nLength; nIndex++)
                {
                    if (aCommands[nIndex].error == undefined)
                        aCommands[nIndex].error = ERR.NO_ERROR;
                }
            }
            */

            return oResponse;
        }
        catch (oErr)
        {
            return {
                success: false,
                error: ERR.INVALID_RESPONSE,
                description: sJson
            };
        }
    }
};