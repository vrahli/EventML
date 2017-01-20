# Scala
S=scala
SC=scalac

# Directories
SRC=src
SRCM=$(SRC)/main
SRCS=$(SRCM)/scala
TARGET=target
CLASSES=$(TARGET)/classes
TST=$(TARGET)/test
ANERISD=aneris
NUPRLLIB=nuprl_lib
JARS=lib/scala-pickling_2.10-0.8.0-SNAPSHOT.jar

# Files
TEST_OUT=process
CONF=$(ANERISD)/conf_aneris_batching.emlc
INPUT1=$(ANERISD)/aneris_batch2_opt4.prog
INPUT2=$(ANERISD)/aneris-batching-control2.prog
ALLDEFS=$(NUPRLLIB)/alldefs_small2

INPUT=$(INPUT1)

# System variables
NPROCS=`grep -c '^processor' /proc/cpuinfo`


# Rules
compile: $(CLASSES)/N2S.class $(CLASSES)/Run.class $(CLASSES)/Client.class

default: compile

$(CLASSES)/Proc.class: $(SRCS)/Proc.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Proc.scala

proc: $(CLASSES)/Proc.class

$(CLASSES)/General.class: $(SRCS)/General.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/General.scala

$(CLASSES)/Location.class: $(SRCS)/Location.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Location.scala

$(CLASSES)/DelayedMessage.class: $(CLASSES)/N2Sprelude.class \
                                 $(SRCS)/DelayedMessage.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/DelayedMessage.scala

$(CLASSES)/Config.class: $(CLASSES)/N2Sprelude.class \
                         $(CLASSES)/N2Sstrings.class \
                         $(CLASSES)/Location.class \
                         $(SRCS)/Config.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Config.scala

$(CLASSES)/N2Sprelude.class: $(SRCS)/N2Sprelude.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/N2Sprelude.scala

$(CLASSES)/N2Sstrings.class: $(SRCS)/N2Sstrings.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/N2Sstrings.scala

$(CLASSES)/N2Sclass.class: $(CLASSES)/N2Sstrings.class \
                           $(CLASSES)/N2Sprelude.class \
                           $(SRCS)/N2Sclass.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/N2Sclass.scala

$(CLASSES)/N2Sparser.class: $(CLASSES)/N2Sclass.class \
                            $(SRCS)/N2Sparser.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/N2Sparser.scala

$(CLASSES)/N2S.class: $(CLASSES)/N2Sclass.class \
                      $(CLASSES)/N2Sparser.class \
                      $(CLASSES)/N2Sprelude.class \
                      $(CLASSES)/N2Sstrings.class \
                      $(CLASSES)/Config.class \
                      $(SRCS)/N2S.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/N2S.scala -deprecation

$(CLASSES)/Connect.class: $(CLASSES)/Config.class \
                          $(CLASSES)/General.class \
                          $(CLASSES)/Location.class \
                          $(CLASSES)/DelayedMessage.class \
                          $(CLASSES)/N2Sprelude.class \
                          $(SRCS)/Connect.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Connect.scala

$(CLASSES)/Client.class: $(CLASSES)/Connect.class \
                         $(CLASSES)/Config.class \
                         $(CLASSES)/N2Sprelude.class \
                         $(CLASSES)/General.class \
                         $(CLASSES)/Location.class \
                         $(SRCS)/Client.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Client.scala

$(CLASSES)/Run.class: $(CLASSES)/Config.class \
                      $(CLASSES)/Connect.class \
                      $(CLASSES)/General.class \
                      $(CLASSES)/DelayedMessage.class \
                      $(CLASSES)/N2Sprelude.class \
                      $(SRCS)/Run.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Run.scala -deprecation


#### ANERIS STUFF

$(CLASSES)/$(TEST_OUT)_lib.class: $(CLASSES)/N2Sprelude.class \
                                  $(TST)/$(TEST_OUT)_lib.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_lib.scala

$(CLASSES)/$(TEST_OUT)_gen.class: $(CLASSES)/N2Sprelude.class \
                                  $(CLASSES)/$(TEST_OUT)_lib.class \
                                  $(TST)/$(TEST_OUT)_gen.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_gen.scala

$(CLASSES)/$(TEST_OUT)_inst.class: $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_gen.class \
                                   $(TST)/$(TEST_OUT)_inst.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_inst.scala

$(CLASSES)/$(TEST_OUT)_rep1.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_rep1.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_rep1.scala

$(CLASSES)/$(TEST_OUT)_rep2.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_rep2.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_rep2.scala

$(CLASSES)/$(TEST_OUT)_ldr1.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_ldr1.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_ldr1.scala

$(CLASSES)/$(TEST_OUT)_ldr2.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_ldr2.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_ldr2.scala

$(CLASSES)/$(TEST_OUT)_acc1.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_acc1.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_acc1.scala

$(CLASSES)/$(TEST_OUT)_acc2.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_acc2.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_acc2.scala

$(CLASSES)/$(TEST_OUT)_acc3.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_acc3.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_acc3.scala

$(CLASSES)/$(TEST_OUT)_loc1.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_loc1.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_loc1.scala

$(CLASSES)/$(TEST_OUT)_loc2.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_loc2.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_loc2.scala

$(CLASSES)/$(TEST_OUT)_loc3.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_loc3.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_loc3.scala

$(CLASSES)/$(TEST_OUT)_loc4.class: $(CLASSES)/Run.class \
                                   $(CLASSES)/N2Sprelude.class \
                                   $(CLASSES)/$(TEST_OUT)_inst.class \
                                   $(TST)/$(TEST_OUT)_loc4.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(TST)/$(TEST_OUT)_loc4.scala

c-aneris: $(CLASSES)/$(TEST_OUT)_rep1.class \
          $(CLASSES)/$(TEST_OUT)_rep2.class \
          $(CLASSES)/$(TEST_OUT)_ldr1.class \
          $(CLASSES)/$(TEST_OUT)_ldr2.class \
          $(CLASSES)/$(TEST_OUT)_acc1.class \
          $(CLASSES)/$(TEST_OUT)_acc2.class \
          $(CLASSES)/$(TEST_OUT)_acc3.class \
          $(CLASSES)/$(TEST_OUT)_loc1.class \
          $(CLASSES)/$(TEST_OUT)_loc2.class \
          $(CLASSES)/$(TEST_OUT)_loc3.class \
          $(CLASSES)/$(TEST_OUT)_loc4.class

.PHONY: aneris
aneris: compile
	# generate test files
	$(S) -classpath $(CLASSES):$(JARS) N2S $(TST)/$(TEST_OUT) $(CONF) $(INPUT) $(ALLDEFS)
	# compile test files
	make -j$(NPROCS) c-aneris
	# Then try running:
	# ./start aneris


#### TESTS

## Test serializer
$(CLASSES)/TestSerializer.class: $(SRCS)/TestSerializer.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/TestSerializer.scala -deprecation

testser: $(CLASSES)/TestSerializer.class

r-testser:
	$(S) -classpath $(CLASSES):$(JARS) TestSerializer

## Compiles test
$(CLASSES)/Test.class: c-aneris \
                       $(CLASSES)/Run.class \
                       $(CLASSES)/General.class \
                       $(CLASSES)/N2Sprelude.class \
                       $(CLASSES)/DelayedMessage.class \
                       $(SRCS)/Test.scala
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Test.scala -deprecation

test: $(CLASSES)/Test.class

## Compiles test by re-generating the processes first
c-test: aneris
	$(SC) -d $(CLASSES) -classpath $(CLASSES):$(JARS) $(SRCS)/Test.scala -deprecation

## Runs test
r-test:
	$(S) -classpath $(CLASSES):$(JARS) Test

## Tests config parser
test-conf: $(CLASSES)/Config.class
	$(S) -classpath $(CLASSES):$(JARS) Config $(CONF)

client: $(CLASSES)/Client.class

## Tests client
r-client:
	scala -classpath $(CLASSES):$(JARS) Client $(CONF) client1

## Test Proc
test-proc:
	scala -classpath $(CLASSES):$(JARS) Proc
