void httpRequest(HttpRequest request, void delegate(Data) resultHandler, void delegate(string) errorHandler, int redirectCount = 0)
{

void responseHandler(HttpResponse response, string disconnectReason)
{
    if (!response)
        if (errorHandler)
            errorHandler(disconnectReason);
        else
            throw new Exception(disconnectReason);
    else
    if (response.status >= 300 && response.status < 400 && "Location" in response.headers)
    {
        if (redirectCount == 15)
            throw new Exception("HTTP redirect loop: " ~ request.url);
        request.resource = applyRelativeURL(request.url, response.headers["Location"]);
        if (response.status == HttpStatusCode.SeeOther)
        {
            request.method = "GET";
            request.data = null;
        }
        httpRequest(request, resultHandler, errorHandler, redirectCount+1);
    }
    else
        if (errorHandler)
            try
                resultHandler(response.getContent());
            catch (Exception e)
                errorHandler(e.msg);
        else
            resultHandler(response.getContent());
}

}
