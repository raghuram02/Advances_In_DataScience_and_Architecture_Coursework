<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="HomePage.aspx.cs" Inherits="WebApplication2_Clustering.HomePage" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
    <div style="height: 434px">
    
        <br />
        User Interface<br />
        <br />
        Enter the Tweet<br />
        <asp:TextBox ID="TextBox4" runat="server"></asp:TextBox>
        <br />
        Enter the Emotion<br />
        <asp:TextBox ID="TextBox3" runat="server"></asp:TextBox>
        <br />
        Enter the
        Polarity<br />
        <asp:TextBox ID="TextBox2" runat="server"></asp:TextBox>
        <br />
        Pol<br />
        <asp:TextBox ID="TextBox1" runat="server"></asp:TextBox>
        <br />
        <br />
        Predicted Sentiment Label<br />
        <asp:Label ID="Label1" runat="server" Text="Label"></asp:Label>
        <br />
        <br />
        <asp:Button ID="Button1" runat="server" OnClick="Button1_Click1" Text="Submit" />
    
    </div>
    </form>
</body>
</html>
