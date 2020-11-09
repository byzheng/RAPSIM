# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   8:50 PM Monday, 17 September 2012
# * Copyright: AS IS
# *

# Component template for sim

#' Get template by component
#' @param component The component name
componentTemplate <- function(component)
{
    template <- NULL
    if (component == 'IrrigationAtFixedDate')
    {
        template <- '
<component name="Irrigate on date" executable="%apsim%\\Model\\Manager2.dll" class="Manager2">
      <executable name="%apsim%/Model/Manager2.dll" version="1.0" />
      <initdata>
        <ui>
          <category type="category" description="Irrigation will be applied on the date(s) below" />
          <sameDays type="yesno" description="Apply Irrigation on the same day(s) each year? (yes/no) - if &quot;no&quot; then must include the year of application below">yes</sameDays>
          <irrigDatesStr type="text" description="Dates for one or more Irrigation applications (dd-mmm or dd-mmm-yyyy) as a list with a space between dates">01-jan 01-jul</irrigDatesStr>
          <category type="category" description="Irrigation parameters" />
          <amount type="text" description="Amount of irrigation to apply (mm)">50</amount>
          <eff description="Irrigation efficiency (0-1)">1.0</eff>
        </ui>
        <text>using System;
using ModelFramework;
using CSGeneral;

public class Script 
{      
   [Link] Irrigation Irrigation; 
   [Input] DateTime today;
   [Param] string[] irrigDatesStr;  //a string array holding the dates as text
   [Param] string sameDays;         //if yes, we ignore the year component
   [Param] float amount;           //amount of irrigation to apply (mm)
   [Param] float eff;              //irrigation efficiency
   
   private DateTime[] irrigDates;   //an array to hold the converted dates
   private bool irrigToday = false; //a variable to hold whether or not we irrigate today
   
   [EventHandler] public void OnInitialised()
   {
      //convert the date strings to APSIM dates
      irrigDates = new DateTime[irrigDatesStr.Length];
      for (int i = 0; i &lt; irrigDates.Length ; i++)
         irrigDates[i] = DateUtility.GetDate(irrigDatesStr[i]);
   }
          
   // The following event handler will be called each day at the start of the day
   [EventHandler] public void OnPrepare()
   {
      irrigToday = false;
      
      //for every date in our date array
      foreach (DateTime day in irrigDates)
      {
         if (sameDays.ToLower().Equals("yes")&amp;&amp; day.Day.Equals(today.Day) &amp;&amp; day.Month.Equals(today.Month))
            irrigToday = true; 
         else if (day.Day.Equals(today.Day) &amp;&amp; day.Month.Equals(today.Month) &amp;&amp; day.Year.Equals(today.Year))
            irrigToday = true;   
      }
      
         if (irrigToday)
         {
            IrrigationApplicationType data = new IrrigationApplicationType();
            data.Amount = amount;
            Irrigation.Set("irrigation_efficiency", eff);
            Irrigation.Apply(data);
         }
   }  
}
       </text>
      </initdata>
    </component>
'
    } else if (component == 'FertiliseAtFixedDate')
    {
        template <- '
    <component name="Fertilise on fixed date" executable="%apsim%\\Model\\Manager.dll" class="Manager">
      <executable name="%apsim%\\Model\\Manager.dll" version="1.0" />
      <initdata>
        <ui>
          <category type="category" description="When should fertiliser be applied" />
          <fert_date type="ddmmmdate" description="Enter fertiliser date (dd-mmm) : ">01-jun</fert_date>
          <fert_criteria type="text" description="Don\'t add fertiliser if N in top 2 layers exceeds (kg/ha) : ">1000</fert_criteria>
          <category type="category" description="Fertiliser application details" />
          <fertmodule type="modulename" description="Module used to apply the fertiliser : ">fertiliser</fertmodule>
          <fert_amount type="text" description="Amount of fertiliser to apply (kg/ha) : ">150</fert_amount>
          <fert_type type="list" listvalues="NO3_N, NH4_N, NH4NO3, urea_N, urea_no3, urea, nh4so4_n, rock_p, banded_p, broadcast_p" description="Fertiliser type : ">urea_N</fert_type>
        </ui>
        <script>
          <text>
         if (today = date(\'[fert_date]\') then
            N_topsoil = no3(1) + nh4(1) + no3(2) + nh4(2)
            if (N_topsoil &lt; [fert_criteria]) then
               [fertmodule] apply amount = [fert_amount] (kg/ha), depth = 50 (mm), type = [fert_type] ()
            endif
         endif </text>
          <event>start_of_day</event>
        </script>
      </initdata>
    </component>
'
    } else if (component == 'FertiliseAtSowing')
    {
        template <- '
    <component name="Fertilise at sowing" executable="%apsim%\\Model\\Manager.dll" class="Manager">
      <executable name="%apsim%\\Model\\Manager.dll" version="1.0" />
      <initdata>
        <ui>
          <category type="category" description="When should fertiliser be applied" />
          <modulename type="modulename" description="On which module should the event come from : ">wheat</modulename>
          <eventname type="text" description="On which event should fertiliser be applied : ">sowing</eventname>
          <category type="category" description="Fertiliser application details" />
          <fertmodule type="modulename" description="Module used to apply the fertiliser : ">fertiliser</fertmodule>
          <fert_amount_sow type="text" description="Amount of starter fertiliser at sowing (kg/ha) : ">150</fert_amount_sow>
          <fert_type_sow type="list" listvalues="NO3_N, NH4_N, NH4NO3, urea_N, urea_no3, urea, nh4so4_n, rock_p, banded_p, broadcast_p" description="Sowing fertiliser type : ">urea_N</fert_type_sow>
        </ui>
        <script>
          <text>
        [fertmodule] apply amount = [fert_amount_sow] (kg/ha), depth = 50 (mm), type = [fert_type_sow]
            </text>
          <event>[modulename].[eventname]</event>
        </script>
      </initdata>
    </component>
'
    } else
    {
        stop(sprintf('Template %s does not support', component))
    }
    template <- readLines(textConnection(template))
    return(template[nchar(template) > 0])
}
