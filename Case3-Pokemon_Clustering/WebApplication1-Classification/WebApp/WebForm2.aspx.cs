using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using CallRequestResponseService;

namespace WebApp
{
    public partial class WebForm2 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {

        }

        protected void Button1_Click1(object sender, EventArgs e)
        {
            PredictPolarityClass p = new PredictPolarityClass();

            p.text = TextBox4.Text;
            string predictPolarity = AzureMachineLearning.PredictPolarity(p);
            Label1.Text = predictPolarity;
        }
    }
}