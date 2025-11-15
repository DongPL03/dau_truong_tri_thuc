package com.app.backend.components;

import com.app.backend.models.NguoiDung;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class SecurityUtils {

    /**
     * 🔹 Lấy đối tượng Authentication hiện tại
     */
    public Authentication getAuthentication() {
        return SecurityContextHolder.getContext().getAuthentication();
    }

    public NguoiDung getLoggedInUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null &&
                authentication.getPrincipal() instanceof NguoiDung selectedUser) {
            if (!selectedUser.isActive()) {
                return null;
            }
            return (NguoiDung) authentication.getPrincipal();
        }
        return null;
    }

    public Long getLoggedInUserIdSafe() {
        try {
            Authentication authentication = getAuthentication();
            if (authentication != null &&
                    authentication.getPrincipal() instanceof NguoiDung selectedUser) {
                if (!selectedUser.isActive()) return null;
                return selectedUser.getId();
            }
        } catch (Exception ignored) {
        }
        return null;
    }

    public Long getLoggedInUserId() {
        Authentication authentication = getAuthentication();
        if (authentication != null &&
                authentication.getPrincipal() instanceof NguoiDung selectedUser) {
            if (!selectedUser.isActive()) {
                return null;
            }
            return selectedUser.getId();
        }
        return null;
    }

//    public static boolean isAdmin() {
//        var auth = SecurityContextHolder.getContext().getAuthentication();
//        return auth != null && auth.getAuthorities().stream().anyMatch(a -> a.getAuthority().equals("ROLE_ADMIN"));
//    }

//    public static void requireAdmin() throws PermissionDenyException {
//        if (!isAdmin()) {
//            throw new PermissionDenyException("Bạn không có quyền ADMIN");
//        }
//    }

    /**
     * 🔹 Lấy danh sách quyền (role) của user hiện tại
     */
    public List<String> getCurrentUserRoles() {
        Authentication auth = getAuthentication();
        if (auth == null) return List.of();
        return auth.getAuthorities()
                .stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.toList());
    }

    /**
     * 🔹 Kiểm tra có role cụ thể không
     */
    public boolean hasRole(String roleName) {
        if (roleName == null) return false;
        Authentication auth = getAuthentication();
        if (auth == null) return false;
        return auth.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .anyMatch(role -> role.equals("ROLE_" + roleName)
                        || role.equals(roleName));
    }

    /**
     * 🔹 Kiểm tra có phải ADMIN không
     */
    public boolean isAdmin() {
        return hasRole("ROLE_ADMIN");
    }

    /**
     * 🔹 Kiểm tra có phải USER không
     */
    public boolean isUser() {
        return hasRole("ROLE_USER");
    }

    /**
     * 🔹 Bắt buộc quyền ADMIN — nếu không thì ném lỗi 403
     *
     * @return
     */
    public boolean requireAdmin() {
        if (!isAdmin()) {
            throw new AccessDeniedException("Bạn không có quyền ADMIN để thực hiện hành động này");
        }
        return false;
    }

    /**
     * 🔹 Kiểm tra xem có ai đăng nhập chưa
     */
    public boolean isAuthenticated() {
        Authentication auth = getAuthentication();
        return auth != null && auth.isAuthenticated()
                && !"anonymousUser".equals(auth.getPrincipal());
    }

    /**
     * 🔹 Bắt buộc đã đăng nhập — nếu không thì ném lỗi 401
     */
    public void requireLogin() {
        if (!isAuthenticated()) {
            throw new AccessDeniedException("Bạn cần đăng nhập để thực hiện hành động này");
        }
    }

    /**
     * 🔹 Xoá context authentication (thường dùng khi logout)
     */
    public void clearContext() {
        SecurityContextHolder.clearContext();
    }
}
