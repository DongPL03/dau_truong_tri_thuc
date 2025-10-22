package com.app.backend.components;
import com.app.backend.models.NguoiDung;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Component
public class SecurityUtils {

    public NguoiDung getLoggedInUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null &&
                authentication.getPrincipal() instanceof NguoiDung selectedUser) {
            if(!selectedUser.isActive()) {
                return null;
            }
            return (NguoiDung) authentication.getPrincipal();
        }
        return null;
    }
}
