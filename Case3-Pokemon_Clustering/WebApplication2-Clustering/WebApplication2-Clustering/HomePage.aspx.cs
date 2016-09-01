using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using CallRequestResponseService;

namespace WebApplication2_Clustering
{
    public partial class HomePage : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {

        }

        protected void Button1_Click1(object sender, EventArgs e)
        {
            PolarityClustering p = new PolarityClustering();
            p.text = TextBox4.Text;
            p.pol = Convert.ToInt16(TextBox1.Text);
            p.emotion = TextBox3.Text;
            p.polarity = TextBox2.Text;

            string predictPolarity = AzureMachineLearning2.PredictPolarityClustering(p);
            Label1.Text = predictPolarity;
 
        }
    }
}