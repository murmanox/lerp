type Lerpable =
	| boolean
	| number
	| Color3
	| CFrame
	| NumberRange
	| NumberSequenceKeypoint
	| PhysicalProperties
	| Ray
	| Rect
	| UDim
	| UDim2
	| Vector2
	| Vector2int16
	| Vector3
	| ColorSequence
	| Region3
	| NumberSequence

declare function lerp<T extends Lerpable>(start_value: T, end_value: T, alpha: number): T
export = lerp
