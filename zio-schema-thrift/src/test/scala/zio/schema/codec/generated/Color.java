/**
 * Autogenerated by Thrift Compiler (0.16.0)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package zio.schema.codec.generated;


@jakarta.annotation.Generated(value = "Autogenerated by Thrift Compiler (0.16.0)")
public enum Color implements org.apache.thrift.TEnum {
  RED(0),
  GREEN(1),
  BLUE(2);

  private final int value;

  private Color(int value) {
    this.value = value;
  }

  /**
   * Get the integer value of this enum value, as defined in the Thrift IDL.
   */
  public int getValue() {
    return value;
  }

  /**
   * Find a the enum type by its integer value, as defined in the Thrift IDL.
   * @return null if the value is not found.
   */
  @org.apache.thrift.annotation.Nullable
  public static Color findByValue(int value) {
    switch (value) {
      case 0:
        return RED;
      case 1:
        return GREEN;
      case 2:
        return BLUE;
      default:
        return null;
    }
  }
}
