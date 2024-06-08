def adjacent_duplicates_to_zero(nums) :
	anslist = []
	anslist.append(nums[0])
	for i in range(len(nums)-1) :
		if nums[i+1] == nums[i]:
			anslist.append(0)
		elif nums[i+1] != nums[i] :
			anslist.append(nums[i+1])
	print(anslist)
	return anslist
adjacent_duplicates_to_zero([1,2,2,3,3,2,2])