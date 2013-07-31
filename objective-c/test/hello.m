#include <stdio.h>
/*
* The next #include line is generally present in all Objective-C
* source files that use GNUstep.  The Foundation.h header file
* includes all the other standard header files you need.
*/
#include <Foundation/Foundation.h>

/*
* declare the Test class implements the class method (classStringValue).
*/
@interface Test
+ (const char *) classStringValue;
@end

/**
* Define the Test class and the class method (classStringValue).
*/
@implementation Test
+ (const char *) classStringValue;
{
    return "this is the string value of the Test class";
}
@end

/**
* main() function: pass a message to the Test class and print the returned string
*/
int main(void)
{
    printf("%s\n", [Test classStringValue]);
    return 0;
}
