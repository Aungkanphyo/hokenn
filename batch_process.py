import csv
import shutil
import time

def process_batch():
    declareFile = 'Data/screen4_output.csv'
    appFile = 'Data/T_Application.csv'
    appFileTmp = 'Data/T_Application_Batch_Tmp.csv'
    declareTmp = 'Data/scree4_tmp.csv'
    auditFile = 'Data/AuditLog.csv'

    pendingUpdate = {}

    try:
        with open(declareFile, 'r', encoding='utf-8') as f_in, \
            open(declareTmp, 'w', encoding='utf-8', newline='') as f_out:
            
            reader = csv.reader(f_in)
            writer = csv.writer(f_out)

            for row in reader:
                if len(row) >= 3:
                    imei = row[0].strip()

                    statusColIndex = None
                    currentStatus = ""
                    totalScore = 0

                    for index in range(2, len(row)):
                        colData = row[index]

                        if ':' in colData:
                            # ':' based on the question name (Key) and score (Value) are separated
                            key, value = colData.split(':', 1)
                            key = key.strip().upper()
                            value = value.strip()

                            # If it contains the word 'STATUS', it will be marked as a Status Column
                            if 'STATUS' in key:
                                statusColIndex = index
                                currentStatus = value.upper()
                            else:
                                # will add up all the numbers in the remaining columns except Status
                                try:
                                    totalScore += int(value)
                                except ValueError:
                                    pass

                    if statusColIndex is not None:
                        if currentStatus == 'REJECTED':
                            pendingUpdate[imei] = 'Rejected'
                            row[statusColIndex] = row[statusColIndex].replace('REJECTED', 'COMPLETED')
                        
                        elif currentStatus == 'APPROVED':
                            pendingUpdate[imei] = 'Accepted'
                            row[statusColIndex] = row[statusColIndex].replace('APPROVED', 'COMPLETED')

                        elif currentStatus == 'PENDING':
                            if totalScore < 70:
                                pendingUpdate[imei] = 'Accepted'
                                row[statusColIndex] = row[statusColIndex].replace('PENDING', 'APPROVED')
                            else:
                                pendingUpdate[imei] = 'Rejected'
                                row[statusColIndex] = row[statusColIndex].replace('PENDING', 'REJECTED')

                writer.writerow(row)

        # Replacing the original screen4_output.csv with the updated data
        shutil.move(declareTmp, declareFile)
    
    except FileNotFoundError:
        print(f"Error: {declareFile} file not found.")
        return
    
    # Updating T_Application.csv using T_Application_Batch_Tmp.csv
    try:
        with open(appFile, 'r', encoding='utf-8') as f_in, \
            open(appFileTmp, 'w', encoding='utf-8', newline='') as f_out:
            
            reader = csv.reader(f_in)
            writer = csv.writer(f_out)

            header = next(reader)
            writer.writerow(header)

            # Searching for header column indexes
            cleanHeader = [h.strip().upper() for h in header]
            try:
                imeiIndex = cleanHeader.index('IMEI')
                statusIndex = cleanHeader.index('STATUS')
                nameIndex = cleanHeader.index('NAME')
                planIndex = cleanHeader.index('PLAN_NAME')
            except ValueError:
                # If the column name is incorrect, set the default index
                imeiIndex = 16
                statusIndex = 8
                nameIndex = 0
                planIndex = 15

            updatedCount = 0
            for row in reader:
                if len(row) > imeiIndex and len(row) > statusIndex:
                    currentImei = row[imeiIndex].strip()
                    status = row[statusIndex].strip().upper()

                    if currentImei in pendingUpdate and status == 'PENDING':
                        row[statusIndex] = pendingUpdate[currentImei]
                        updatedCount += 1

                        # Check UPDATE_PENDING and accept the change and add it to the AuditLog
                    elif status.startswith('UPDATE_PENDING'):
                        # Extracting Old Plan from status (e.g. "UPDATE_PENDING:Standard")
                        if ':' in status:
                            _,oldPlan = status.split(':', 1)
                            oldPlan = oldPlan.strip()
                        else:
                            oldPlan = 'Unknown'
                        
                        newPlan = row[planIndex].strip()
                        row[statusIndex] = 'Accepted'
                        updatedCount += 1

                        # Getting the current date and time
                        currentDate = time.strftime('%Y/%m/%d')
                        currentTime = time.strftime('%X')
                        name = row[nameIndex].strip() if len(row) > nameIndex else ""

                        # Save as a new row at the bottom of the AuditLog.csv file
                        try:
                            with open(auditFile, 'a', encoding='utf-8', newline='') as f_audit:
                                auditWriter = csv.writer(f_audit)
                                auditWriter.writerow([currentDate, currentTime, currentImei, name, oldPlan, newPlan, 'PLAN_CHANGED'])
                        except Exception as e:
                            print(f"Error writing to AuditLog: {e}")

                writer.writerow(row)

        shutil.move(appFileTmp, appFile)
        print(f"[{time.strftime('%X')}] success. (New Status Change in T_Application: {updatedCount})")        

    except FileNotFoundError:
        print(f"Error: {appFile} file not found.")    


# To run a batch in a loop every 30 seconds
if __name__=="__main__":
    print("Batch program has started running...")
    while True:
        time.sleep(30)
        process_batch()