package com.app.backend.services.nguoidung;


import com.app.backend.components.JwtTokenUtils;
import com.app.backend.components.LocalizationUtils;
import com.app.backend.dtos.UpdateUserDTO;
import com.app.backend.dtos.UserDTO;
import com.app.backend.dtos.UserLoginDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.ExpiredTokenException;
import com.app.backend.exceptions.InvalidPasswordException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.EmailVerificationToken;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.Token;
import com.app.backend.models.VaiTro;
import com.app.backend.models.constant.TrangThaiNguoiDung;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.repositories.ITokenRepository;
import com.app.backend.repositories.IVaiTroRepository;
import com.app.backend.services.token.ITokenService;
import com.app.backend.services.token.TokenService;
import com.app.backend.services.verifyemail.EmailVerificationService;
import com.app.backend.utils.MessageKeys;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.app.backend.utils.ValidationUtils.isValidEmail;

import java.security.SecureRandom;


@RequiredArgsConstructor
@Service
public class NguoiDungService implements INguoiDungService {
    private final INguoiDungRepository userRepository;
    private final IVaiTroRepository roleRepository;
    private final ITokenRepository tokenRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtTokenUtils jwtTokenUtil;
    private final AuthenticationManager authenticationManager;
    private final LocalizationUtils localizationUtils;
    private final TokenService tokenService;
    private final EmailVerificationService emailVerificationService;


    private String generateRandomDisplayName() {
        // Bộ ký tự cho phép: Chữ hoa, chữ thường và số
        String allowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        SecureRandom random = new SecureRandom();
        StringBuilder sb = new StringBuilder(8); // Độ dài chuỗi random, ví dụ 8 ký tự

        for (int i = 0; i < 8; i++) {
            int index = random.nextInt(allowedChars.length());
            sb.append(allowedChars.charAt(index));
        }

        // Kết quả ví dụ: "User_a1b2C3d4" hoặc chỉ "a1b2C3d4" tùy bạn chọn
        return "User_" + sb.toString();
    }

    @Override
    @Transactional
    public NguoiDung createUser(UserDTO userDTO) throws Exception {
        //register user
        if (!userDTO.getTenDangNhap().isBlank() && userRepository.existsNguoiDungByTenDangNhap(userDTO.getTenDangNhap())) {
            throw new DataNotFoundException(localizationUtils.getLocalizedMessage(MessageKeys.USERNAME_ALREADY_EXISTS));
        }
        if (!userDTO.getEmail().isBlank() && userRepository.existsNguoiDungByEmail(userDTO.getEmail())) {
            throw new DataIntegrityViolationException("Email already exists");
        }
        VaiTro role = roleRepository.findById(userDTO.getRoleId())
                .orElseThrow(() -> new DataNotFoundException(
                        localizationUtils.getLocalizedMessage(MessageKeys.ROLE_DOES_NOT_EXISTS)));


        if (role.getTenVaiTro().equalsIgnoreCase(VaiTro.ADMIN)) {
            throw new PermissionDenyException("Registering admin accounts is not allowed");
        }
        String finalDisplayName = generateRandomDisplayName();
        //convert from userDTO => user
        NguoiDung newUser = NguoiDung.builder()
                .tenDangNhap(userDTO.getTenDangNhap())
                .email(userDTO.getEmail())
                .password(userDTO.getPassword())
                .tenHienThi(finalDisplayName)
                .active(true) // sau khi lên frontend thif sửa lại false
                .build();

        newUser.setVaiTro(role);

//        if (!userDTO.isSocialLogin()) {
        String password = userDTO.getPassword();
        String encodedPassword = passwordEncoder.encode(password);
        newUser.setPassword(encodedPassword);
//        }
        NguoiDung savedUser = userRepository.save(newUser);
//        if (savedUser.getEmail() != null && !savedUser.getEmail().isBlank()) {
//            try {
//                emailVerificationService.createAndSendTokenForUser(savedUser.getId());
//            } catch (Exception e) {
//                // Nếu lỗi khi gửi mail, có thể log lại hoặc throw ra
//                System.err.println("Failed to send verification email: " + e.getMessage());
//            }
//        }
        return savedUser;
    }


    @Override
    public String login(UserLoginDTO userLoginDTO) throws Exception {
        Optional<NguoiDung> optionalUser = Optional.empty();

        // Tìm theo username hoặc email
        if (userLoginDTO.getTenDangNhap() != null && !userLoginDTO.getTenDangNhap().isBlank()) {
            optionalUser = userRepository.findNguoiDungByTenDangNhap(userLoginDTO.getTenDangNhap());
        }
        if (optionalUser.isEmpty() && userLoginDTO.getEmail() != null) {
            optionalUser = userRepository.findByEmail(userLoginDTO.getEmail());
        }

        if (optionalUser.isEmpty()) {
            throw new DataNotFoundException("Wrong phone number or password");
        }

        NguoiDung existingUser = optionalUser.get();

        // Kiểm tra trạng thái tài khoản
        if (!existingUser.isActive()) {
            throw new DataNotFoundException("User is locked");
        }

        if (existingUser.isXoa()) {
            throw new DataNotFoundException("This account has been deactivated.");
        }

        // ✅ Thêm bước kiểm tra password
        if (!passwordEncoder.matches(userLoginDTO.getPassword(), existingUser.getPassword())) {
            throw new DataNotFoundException("Wrong phone number or password");
        }

        existingUser.setTrangThai(TrangThaiNguoiDung.ONLINE);
        existingUser.setDangNhapLanCuoi(Instant.now());
        userRepository.save(existingUser);


        // ✅ Tạo token hợp lệ
        return jwtTokenUtil.generateToken(existingUser);
    }


//    @Override
//    public String loginSocial(UserLoginDTO userLoginDTO) throws Exception {
//        Optional<User> optionalUser = Optional.empty();
//        Role roleUser = roleRepository.findByName(Role.USER)
//                .orElseThrow(() -> new DataNotFoundException(
//                        localizationUtils.getLocalizedMessage(MessageKeys.ROLE_DOES_NOT_EXISTS)));
//
//        // Kiểm tra Google Account ID
//        if (userLoginDTO.isGoogleAccountIdValid()) {
//            optionalUser = userRepository.findByGoogleAccountId(userLoginDTO.getGoogleAccountId());
//
//            // Tạo người dùng mới nếu không tìm thấy
//            if (optionalUser.isEmpty()) {
//                User newUser = User.builder()
//                        .fullName(Optional.ofNullable(userLoginDTO.getFullname()).orElse(""))
//                        .email(Optional.ofNullable(userLoginDTO.getEmail()).orElse(""))
//                        .profileImage(Optional.ofNullable(userLoginDTO.getProfileImage()).orElse(""))
//                        .role(roleUser)
//                        .googleAccountId(userLoginDTO.getGoogleAccountId())
//                        .password("") // Mật khẩu trống cho đăng nhập mạng xã hội
//                        .active(true)
//                        .build();
//
//                // Lưu người dùng mới
//                newUser = userRepository.save(newUser);
//                optionalUser = Optional.of(newUser);
//            }
//        }
//        // Kiểm tra Facebook Account ID
//        else if (userLoginDTO.isFacebookAccountIdValid()) {
//            optionalUser = userRepository.findByFacebookAccountId(userLoginDTO.getFacebookAccountId());
//
//            // Tạo người dùng mới nếu không tìm thấy
//            if (optionalUser.isEmpty()) {
//                User newUser = User.builder()
//                        .fullName(Optional.ofNullable(userLoginDTO.getFullname()).orElse(""))
//                        .email(Optional.ofNullable(userLoginDTO.getEmail()).orElse(""))
//                        .profileImage(Optional.ofNullable(userLoginDTO.getProfileImage()).orElse(""))
//                        .role(roleUser)
//                        .facebookAccountId(userLoginDTO.getFacebookAccountId())
//                        .password("") // Mật khẩu trống cho đăng nhập mạng xã hội
//                        .active(true)
//                        .build();
//
//                // Lưu người dùng mới
//                newUser = userRepository.save(newUser);
//                optionalUser = Optional.of(newUser);
//            }
//        } else {
//            throw new IllegalArgumentException("Invalid social account information.");
//        }
//
//        User user = optionalUser.get();
//
//        // Kiểm tra nếu tài khoản bị khóa
//        if (!user.isActive()) {
//            throw new DataNotFoundException(localizationUtils.getLocalizedMessage(MessageKeys.USER_IS_LOCKED));
//        }
//
//        // Tạo JWT token cho người dùng
//        return jwtTokenUtil.generateToken(user);
//    }

    @Transactional
    @Override
    public NguoiDung updateUser(Long userId, UpdateUserDTO updatedUserDTO) throws Exception {
        // Find the existing user by userId
        NguoiDung existingUser = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));

        // Check if the phone number is being changed and if it already exists for another user
        /*
        String newPhoneNumber = updatedUserDTO.getPhoneNumber();
        if (!existingUser.getPhoneNumber().equals(newPhoneNumber) &&
                userRepository.existsByPhoneNumber(newPhoneNumber)) {
            throw new DataIntegrityViolationException("Phone number already exists");
        }
       */
        // Update user information based on the DTO
        if (updatedUserDTO.getHoTen() != null) {
            existingUser.setHoTen(updatedUserDTO.getHoTen());
        }

        if (updatedUserDTO.getTenHienThi() != null) {
            existingUser.setTenHienThi(updatedUserDTO.getTenHienThi());
        }

        if (updatedUserDTO.getDiaChi() != null) {
            existingUser.setDiaChi(updatedUserDTO.getDiaChi());
        }
//        if (updatedUserDTO.getDateOfBirth() != null) {
//            existingUser.setDateOfBirth(updatedUserDTO.getDateOfBirth());
//        }
//        if (updatedUserDTO.isFacebookAccountIdValid()) {
//            existingUser.setFacebookAccountId(updatedUserDTO.getFacebookAccountId());
//        }
//        if (updatedUserDTO.isGoogleAccountIdValid()) {
//            existingUser.setGoogleAccountId(updatedUserDTO.getGoogleAccountId());
//        }


        // Update the password if it is provided in the DTO
        if (updatedUserDTO.getPassword() != null
                && !updatedUserDTO.getPassword().isEmpty()) {
            if (!updatedUserDTO.getPassword().equals(updatedUserDTO.getRetypePassword())) {
                throw new DataNotFoundException("Password and retype password not the same");
            }
            String newPassword = updatedUserDTO.getPassword();
            String encodedPassword = passwordEncoder.encode(newPassword);
            existingUser.setPassword(encodedPassword);
        }
        //existingUser.setRole(updatedRole);
        // Save the updated user
        return userRepository.save(existingUser);
    }

    @Override
    public NguoiDung getUserById(Long userId) throws Exception {
        return userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
    }

    @Override
    public NguoiDung getUserDetailsFromToken(String token) throws Exception {
        if (jwtTokenUtil.isTokenExpired(token)) {
            throw new ExpiredTokenException("Token is expired");
        }
        String subject = jwtTokenUtil.getSubject(token);
        Optional<NguoiDung> user;
        user = userRepository.findNguoiDungByTenDangNhap(subject);
        if (user.isEmpty() && isValidEmail(subject)) {
            user = userRepository.findByEmail(subject);
        }
        return user.orElseThrow(() -> new Exception("User not found"));
    }

    @Override
    public NguoiDung getUserDetailsFromRefreshToken(String refreshToken) throws Exception {
        Token existingToken = tokenRepository.findByRefreshToken(refreshToken);
        return getUserDetailsFromToken(existingToken.getToken());
    }

    @Override
    public Page<NguoiDung> findAll(String keyword, Pageable pageable) {
        return userRepository.findAll(keyword, pageable);
    }

    @Override
    @Transactional
    public void resetPassword(Long userId, String newPassword)
            throws InvalidPasswordException, DataNotFoundException {
        NguoiDung existingUser = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
        String encodedPassword = passwordEncoder.encode(newPassword);
        existingUser.setPassword(encodedPassword);
        userRepository.save(existingUser);
        //reset password => clear token
        List<Token> tokens = tokenRepository.findByNguoiDung(existingUser);
        for (Token token : tokens) {
            tokenRepository.delete(token);
        }
    }

    @Override
    @Transactional
    public void blockOrEnable(Long userId, Boolean active) throws DataNotFoundException {
        NguoiDung existingUser = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
        existingUser.setActive(active);
        userRepository.save(existingUser);
    }

    @Override
    @Transactional
    public void changeProfileImage(Long userId, String imageName) throws Exception {
        NguoiDung existingUser = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
        existingUser.setAvatarUrl(imageName);
        userRepository.save(existingUser);
    }
    /**
     * Lấy thông tin user từ refresh token (đang lưu DB) & kiểm tra revoked/expired.
     */
//    public NguoiDung getUserDetailsFromRefreshToken(String refreshToken) throws Exception {
//        return tokenService.getUserFromRefreshToken(refreshToken);
//    }

    /**
     * User tự đổi mật khẩu: kiểm tra oldPassword trước khi set newPassword.
     * Đồng thời có thể revoke toàn bộ token ở controller.
     */
    @Transactional
    public void changePasswordWithOldCheck(Long userId, String oldPassword, String newPassword) throws Exception {
        NguoiDung user = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));

        if (!passwordEncoder.matches(oldPassword, user.getPassword())) {
            throw new IllegalArgumentException("Old password incorrect");
        }
        user.setPassword(passwordEncoder.encode(newPassword));
        userRepository.save(user);
    }

    /**
     * ADMIN cập nhật vai trò user
     */
    @Transactional
    public void updateRole(Long userId, String roleName) throws Exception {
        NguoiDung user = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));

        VaiTro role = roleRepository.findByTenVaiTro(roleName);
        user.setVaiTro(role);
        userRepository.save(user);

        // Nếu muốn an toàn, revoke tất cả token khi hạ quyền
//        if (!"ADMIN".equalsIgnoreCase(roleName)) {
//            tokenService.revokeAllTokensForUser(user);
//        }
        if (role.getTenVaiTro().equalsIgnoreCase(VaiTro.ADMIN)) {
            tokenService.revokeAllTokensForUser(user);
        }
    }

    @Override
    @Transactional
    public void softDeleteUser(Long userId) throws Exception {
        NguoiDung user = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));

        if (user.isXoa()) {
            throw new IllegalStateException("User already deactivated");
        }

        user.setXoa(true);
        user.setActive(false); // vô hiệu hóa luôn
        userRepository.save(user);
    }

    @Override
    @Transactional
    public void restoreUser(Long userId) throws Exception {
        NguoiDung user = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
        user.setXoa(false);
        user.setActive(true);
        userRepository.save(user);
    }

    @Override
    public List<NguoiDung> findByDeletedFalse() {
        return null;
    }

    @Override
    public Optional<NguoiDung> findByIdAndDeletedFalse(Long id) {
        return Optional.empty();
    }

    @Override
    public Optional<NguoiDung> findByEmailAndDeletedFalse(String email) {
        return Optional.empty();
    }

    @Override
    public Long findIdVaiTroByTenDangNhap(String tenDangNhap) {
        return userRepository.findIdVaiTroByTenDangNhap(tenDangNhap);
    }

    // ================== ADMIN METHODS ==================

    @Override
    public Map<String, Object> getAdminUserStats() {
        Map<String, Object> stats = new HashMap<>();

        stats.put("totalUsers", userRepository.count());
        stats.put("activeUsers", userRepository.countActive());
        stats.put("blockedUsers", userRepository.countBlocked());
        stats.put("deletedUsers", userRepository.countDeleted());
        stats.put("totalAdmins", userRepository.countAdmins());

        // User đăng ký hôm nay
        Instant startOfDay = LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant();
        stats.put("todayRegistrations", userRepository.countRegisteredToday(startOfDay));

        return stats;
    }

    @Override
    public Page<NguoiDung> findAllForAdmin(String keyword, Pageable pageable) {
        return userRepository.findAllForAdmin(keyword, pageable);
    }

    @Override
    @Transactional
    public void adminSoftDeleteUser(Long userId) throws Exception {
        NguoiDung user = userRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("User not found"));
        user.setXoa(true);
        userRepository.save(user);

        // Revoke tất cả token của user
        tokenService.revokeAllTokensForUser(user);
    }

    @Override
    public String exportUsersCsv(String keyword) {
        List<NguoiDung> users = userRepository.findAllForExport(keyword);

        StringBuilder csv = new StringBuilder();
        // Header
        csv.append("ID,Tên đăng nhập,Họ tên,Email,Vai trò,Trạng thái,Ngày tạo\n");

        for (NguoiDung u : users) {
            String status = u.isXoa() ? "Đã xóa" : (u.isActive() ? "Đang hoạt động" : "Bị khóa");
            String role = u.getVaiTro() != null ? u.getVaiTro().getTenVaiTro() : "";
            String createdAt = u.getTaoLuc() != null ? u.getTaoLuc().toString() : "";

            csv.append(String.format("%d,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n",
                    u.getId(),
                    escapeCsv(u.getTenDangNhap()),
                    escapeCsv(u.getHoTen()),
                    escapeCsv(u.getEmail()),
                    role,
                    status,
                    createdAt
            ));
        }

        return csv.toString();
    }

    private String escapeCsv(String value) {
        if (value == null) return "";
        return value.replace("\"", "\"\"");
    }
}







