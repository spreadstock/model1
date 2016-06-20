/**
 * 
 */
package com.clean;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;

/**
 * @author exubixu
 */
public class LogUtil
{
    private static final Logger LOGGER = Logger.getLogger(LogUtil.class);
    static
    {
        PropertyConfigurator.configure("conf/log4j.properties");
    }


    public static Logger getLogger()
    {
        return LOGGER;
    }

}
