
#import "AppDelegate.h"
#import "EJJavaScriptView.h"
#import "EJAppViewController.h"
#import "ABYContextManager.h"
#import "ABYServer.h"

@interface AppDelegate ()
@property (strong, nonatomic) ABYContextManager* contextManager;
@property (strong, nonatomic) ABYServer* replServer;
@end

@implementation AppDelegate
@synthesize window;

#pragma mark -
#pragma mark Application lifecycle

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    
    // Optionally set the idle timer disabled, this prevents the device from sleep when
    // not being interacted with by touch. ie. games with motion control.
    application.idleTimerDisabled = YES;
    
    window = [[UIWindow alloc] initWithFrame:UIScreen.mainScreen.bounds];
    [self loadViewControllerWithScriptAtPath:@"index.js"];
    
    [window makeKeyAndVisible];
    
    
    EJJavaScriptView* appView = (EJJavaScriptView*)window.rootViewController.view;
    JSGlobalContextRef ctx = appView.jsGlobalContext;
    
    NSURL* compilerOutputDirectory = [[self privateDocumentsDirectory] URLByAppendingPathComponent:@"cljs-out"];
    [self createDirectoriesUpTo:compilerOutputDirectory];
    
    // Copy resources from bundle "out" to compilerOutputDirectory
    
    /* NSFileManager* fileManager = [NSFileManager defaultManager];
     fileManager.delegate = self;
     
     // First blow away old compiler output directory
     [fileManager removeItemAtPath:compilerOutputDirectory.path error:nil];
     
     // Copy files from bundle to compiler output driectory
     NSString *outPath = [[NSBundle mainBundle] pathForResource:@"out" ofType:nil];
     [fileManager copyItemAtPath:outPath toPath:compilerOutputDirectory.path error:nil];
     */
    
    // Set up our context
    self.contextManager = [[ABYContextManager alloc] initWithContext:ctx
                                             compilerOutputDirectory:compilerOutputDirectory];
    [self.contextManager setupGlobalContext];
    //[self.contextManager setUpExceptionLogging];
    [self.contextManager setUpConsoleLog];
    [self.contextManager setUpTimerFunctionality];
    [self.contextManager setUpAmblyImportScript];
    
    /*NSURL* googDirectory = [compilerOutputDirectory URLByAppendingPathComponent:@"goog"];
     
     [self.contextManager bootstrapWithDepsFilePath:[[compilerOutputDirectory URLByAppendingPathComponent:@"main" isDirectory:NO] URLByAppendingPathExtension:@"js"].path
     googBasePath:[[googDirectory URLByAppendingPathComponent:@"base" isDirectory:NO] URLByAppendingPathExtension:@"js"].path];*/
    
    self.replServer = [[ABYServer alloc] initWithContext:self.contextManager.context
                                 compilerOutputDirectory:compilerOutputDirectory];
    BOOL successful = [self.replServer startListening];
    if (!successful) {
        NSLog(@"Failed to start REPL server.");
    } else {
        NSLog(@"Started REPL server.");
    }
    
    
    return YES;
}

- (void)loadViewControllerWithScriptAtPath:(NSString *)path {
	// Release any previous ViewController
	window.rootViewController = nil;
	
	EJAppViewController *vc = [[EJAppViewController alloc] initWithScriptAtPath:path];
	window.rootViewController = vc;
	[vc release];
}

- (NSURL *)privateDocumentsDirectory
{
    NSURL *libraryDirectory = [[[NSFileManager defaultManager] URLsForDirectory:NSLibraryDirectory inDomains:NSUserDomainMask] lastObject];
    
    return [libraryDirectory URLByAppendingPathComponent:@"Private Documents"];
}

- (void)createDirectoriesUpTo:(NSURL*)directory
{
    if (![[NSFileManager defaultManager] fileExistsAtPath:[directory path]]) {
        NSError *error = nil;
        
        if (![[NSFileManager defaultManager] createDirectoryAtPath:[directory path]
                                       withIntermediateDirectories:YES
                                                        attributes:nil
                                                             error:&error]) {
            NSLog(@"Can't create directory %@ [%@]", [directory path], error);
            abort();
        }
    }
}

- (BOOL)fileManager:(NSFileManager *)fileManager shouldProceedAfterError:(NSError *)error copyingItemAtPath:(NSString *)srcPath toPath:(NSString *)dstPath{
    if ([error code] == 516) //error code for: The operation couldn’t be completed. File exists
        return YES;
    else
        return NO;
}

#pragma mark -
#pragma mark Memory management

- (void)dealloc {
	window.rootViewController = nil;
	[window release];
    [super dealloc];
}


@end
