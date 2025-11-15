package com.app.backend.configurations;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.*;

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
                // ✅ Cho phép FE Angular 4200 truy cập
                .setAllowedOriginPatterns(
                        "http://localhost:*"
                )
                .withSockJS(); // fallback nếu browser không hỗ trợ WS
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {
        // Client gửi tin → /app/**
        registry.setApplicationDestinationPrefixes("/app");

        // Server gửi broadcast → /topic/**
        registry.enableSimpleBroker("/topic");

        // Nếu gửi riêng user
        registry.setUserDestinationPrefix("/user");
    }
}
